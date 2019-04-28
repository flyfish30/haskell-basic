-- Parse the source code of opencl kernel file, and get kernel function declarations.
-- Copyright (c) 2019 Parker Liu<liuchangsheng30@gmail.com>
-- Created time is 2019-4-24
-- Referenced from http://stackoverflow.com/questions/6289950/
--

module Main where

import System.IO
import System.IO.Unsafe
import System.Environment

import Control.Monad
import Control.Arrow

import Language.C
import Language.C.Analysis
import Language.C.Analysis.SemRep
import Language.C.System.GCC

data PsicopBaseType = PsicopVoid | PsicopInt8 | PsicopInt16 | PsicopInt32 | PsicopInt64 | PsicopFloat | PsicopDouble deriving Eq
data PsicopType = PsicopType Bool PsicopBaseType deriving Eq

instance Show PsicopBaseType where
        show PsicopVoid = "0"
        show PsicopInt8 = "PSICOP_TYPE_INT8"
        show PsicopInt16 = "PSICOP_TYPE_INT16"
        show PsicopInt32 = "PSICOP_TYPE_INT32"
        show PsicopInt64 = "PSICOP_TYPE_INT64"
        show PsicopFloat = "PSICOP_TYPE_FLOAT"
        show PsicopDouble = "PSICOP_TYPE_DOUBLE"

instance Show PsicopType where
        show (PsicopType isPtr typ) = (show typ) ++ (if isPtr
                                                        then " | PSICOP_TYPE_POINTER"
                                                        else "")

data MyFunDef = MyFunDef String PsicopType [PsicopType] deriving Show
type State = [MyFunDef]

asS :: (Pretty a) => a -> String
asS = show . pretty

psicopIntType :: IntType -> PsicopBaseType
psicopIntType TyBool = PsicopInt32
psicopIntType TyChar = PsicopInt8
psicopIntType TySChar = PsicopInt8
psicopIntType TyUChar = PsicopInt8
psicopIntType TyShort = PsicopInt16
psicopIntType TyUShort = PsicopInt16
psicopIntType TyInt = PsicopInt32
psicopIntType TyUInt = PsicopInt32
psicopIntType TyLong = PsicopInt32
psicopIntType TyULong = PsicopInt32
psicopIntType TyLLong = PsicopInt64
psicopIntType TyULLong = PsicopInt64

psicopType :: Type -> Either String PsicopType
psicopType (DirectType TyVoid _ _) = Right $ PsicopType False PsicopVoid
psicopType (DirectType (TyIntegral intType) _ _) = Right $ PsicopType False (psicopIntType intType)
psicopType (DirectType (TyFloating TyFloat) _ _) = Right $ PsicopType False PsicopFloat
psicopType (DirectType (TyFloating TyDouble) _ _) = Right $ PsicopType False PsicopDouble
psicopType (DirectType (TyEnum e) _ _) = Right $ PsicopType False PsicopInt32
psicopType (TypeDefType (TypeDefRef i t _) _ _) = psicopType t
psicopType (PtrType t _ _) = let innerType = psicopType t
                             in case innerType of
                                  (Left _) -> Right $ PsicopType True PsicopVoid
                                  (Right (PsicopType isPtr typ)) ->
                                       if isPtr
                                       then Left "Pointer to pointer declaration detected"
                                       else Right $ PsicopType True typ
psicopType t = Left ("Unknown type " ++ (asS t))

makeFunDef :: String -> Type -> [ParamDecl] -> Bool -> Attributes -> Maybe MyFunDef
makeFunDef name returnType params isVaradic attrs =
  let rt = psicopType returnType
  in case rt of
        Left s -> Nothing
        Right t -> let ps = checkTypes (map getType params)
                   in case ps of
                        Nothing -> Nothing
                        Just l -> Just $ MyFunDef name t l
 where
getType :: ParamDecl -> Either String PsicopType
getType (ParamDecl v _) = let (VarDecl varName declAttr t) = getVarDecl v
                              in psicopType t
getType t = Left "Unknown paramater"
checkTypes :: [Either String PsicopType] -> Maybe [PsicopType]
checkTypes [] = Just []
checkTypes (x:xs) = case x of
                      Left err -> Nothing
                      Right t -> let rest = checkTypes xs
                                 in case rest of
                                      Nothing -> Nothing
                                      Just r -> Just (t : r)

handler :: DeclEvent -> Trav State ()
handler (DeclEvent (FunctionDef fd)) =
        do error "FunctionDef not implemented"
           return ()
handler (DeclEvent (Declaration d)) =
        do
        let (VarDecl varName declAttr t) = getVarDecl d
        case t of
             (FunctionType (FunType returnType params isVaradic) attrs) ->
                do let fun = (makeFunDef (asS varName) returnType params isVaradic attrs)
                        in case fun of
                                Nothing -> do return ()
                                Just x -> do modifyUserState (\s -> x : s)
                                             return ()
             _ -> do return ()
        return ()
handler _ =
        do return ()

newState :: State
newState = []

header :: String -> String
header name =
  "static void " ++ name ++ "_register(void) {\n" ++
  "\tstatic PSICOPFunctionInfo functions[] = {\n"

stub :: String -> String
stub name =
        ".globl " ++ name ++ "\n" ++
        ".text\n" ++
        ".type " ++ name ++ ", @function\n" ++
        name ++ ":\n" ++
        "\tmovl (" ++ name ++ "_ptr), %eax\n" ++
        "\tcmpl $0, %eax\n" ++
        "\tjne 1f\n" ++
        "\tleal " ++ name ++ "_text, %eax\n" ++
        "\tcall getfunc\n" ++
        "\tmovl %eax, (" ++ name ++ "_ptr)\n" ++
        "1:\n" ++
        "\tjmp *%eax\n" ++
        "\tret\n" ++
        "\n" ++
        ".data\n" ++
        name ++ "_ptr: .long 0\n" ++
        name ++ "_text: .asciz \"" ++ name ++ "\"\n\n"


footer :: [String] -> String
footer funNames =
        "\t};\n" ++
        "\tsize_t i;\n" ++
        "\n" ++
        "\tfor (i = 0; i < sizeof(functions)/sizeof(functions[0]); i++) {\n" ++
        "\t\tpsicop_register_function(&functions[i]);\n" ++
        "\t}\n" ++
        "}\n" ++
        "\n" ++
        "\n/*\n" ++
        "\n" ++
        (concatMap stub funNames) ++
        "\n" ++
        ".text\n" ++
        ".extern psicop_getfunc\n" ++
        ".extern exit\n" ++
        ".align 4\n" ++
        "getfunc:\n" ++
        "\tpushl %eax\n" ++
        "\tcall psicop_getfunc\n" ++
        "\torl %eax, %eax\n" ++
        "\tjz 1f\n" ++
        "\taddl $4, %esp\n" ++
        "\tret\n" ++
        "1:\n" ++
        "\t# function name still on stack\n" ++
        "\tleal error_text, %eax\n" ++
        "\tpushl %eax\n" ++
        "\tcall printf\n" ++
        "\tpushl $60\n" ++
        "\tcall exit\n" ++
        "\n" ++
        ".data\n" ++
        "error_text: .asciz \"Could not find function: '%s'\\n\"\n" ++
        "*/\n\n"

outputFunDef :: MyFunDef -> String
outputFunDef (MyFunDef name returnType params) =
  "\t{ DEFUN(" ++ name ++ ", " ++ (show returnType) ++ ", " ++ (show $ length params) ++ ") { " ++ (unwords $ map (\x -> (show x) ++ ",") params) ++ " } },\n"

errToString :: CError -> String
errToString (CError err) = show err

main :: IO ()
main = do
    let usage = error "give file to parse"
    (opts,c_file) <- liftM (init &&& last) getArgs

    let compiler = newGCC "gcc"
    ast <- parseCFile compiler Nothing opts c_file >>= checkResult "[parsing]"

    case (runTrav newState (withExtDeclHandler (analyseAST ast) handler)) of
         Left errs -> do putStrLn ("errors: " ++ concat (map errToString errs))
         Right (decls, state) -> do putStr $ header (takeWhile (/='.') c_file)
                                    mapM_ (putStr . outputFunDef) (userState state)
                                    putStr $ footer (map funName (userState state))

    where
    checkResult :: (Show a) => String -> (Either a b) -> IO b
    checkResult label = either (error . (label++) . show) return

    funName :: MyFunDef -> String
    funName (MyFunDef name _ _) = name
