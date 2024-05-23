module FMinus

open AST
open Types

// Evaluate expression into a value, under the given environment.
let rec evalExp (exp: Exp) (env: Env) : Val =
 match exp with
  | Num i -> Int i // 숫자인 경우 int 값으로 반환
  | True -> Bool true // true인 경우 Bool 값으로 반환
  | False -> Bool false // false인 경우 Bool 값으로 반환
  | Var v -> // 변수의 경우
      match Map.tryFind v env with
      | Some value -> value // 변수가 환경에 존재하면 해당 값 반환
      | None -> raise UndefinedSemantics // 예외
  | Neg n -> evalExp n env |> negateValue // 부정 연산
  | Add (e1, e2) -> evalBinOp e1 e2 env (+) // 덧셈 연산
  | Sub (e1, e2) -> evalBinOp e1 e2 env (-) // 뺄셈 연산
  | App (e1, e2) -> evalApp e1 e2 env // 함수
  | LessThan (e1, e2) -> evalRelOp e1 e2 env (<) // < 연산
  | GreaterThan (e1, e2) -> evalRelOp e1 e2 env (>) // > 연산
  | Equal (e1, e2) -> evalEquality e1 e2 env // = 연산
  | NotEq (e1, e2) -> evalInequality e1 e2 env // != 연산
  | IfThenElse (e1, e2, e3) -> evalConditional e1 e2 e3 env // 조건문
  | LetIn (v, e1, e2) -> evalLet v e1 e2 env // Let
  | LetFunIn (f, x, e1, e2) -> evalLetFun f x e1 e2 env // 함수 정의와 Let
  | LetRecIn (f, x, e1, e2) -> evalLetRec f x e1 e2 env // 재귀 함수와 Let
  | Fun (arg, body) -> Func(arg, body, env) // 함수 정의
  | _ -> raise UndefinedSemantics // 예외

and negateValue value =
  match value with
  | Int i -> Int (-i)
  | _ -> raise UndefinedSemantics

and evalBinOp e1 e2 env op =
  match evalExp e1 env, evalExp e2 env with
  | Int i1, Int i2 -> Int (op i1 i2)
  | _ -> raise UndefinedSemantics

and evalApp e1 e2 env =
  match evalExp e1 env, evalExp e2 env with
  | Func(x, body, appEnv), argVal ->
      let newEnv = Map.add x argVal appEnv
      evalExp body newEnv
  | RecFunc(f, x, body, appEnv), argVal ->
      let newEnv = Map.add x argVal (Map.add f (RecFunc(f, x, body, appEnv)) appEnv)
      evalExp body newEnv
  | _ -> raise UndefinedSemantics

and evalRelOp e1 e2 env op =
  match evalExp e1 env, evalExp e2 env with
  | Int i1, Int i2 -> Bool (op i1 i2)
  | _ -> raise UndefinedSemantics

and evalEquality e1 e2 env =
  match evalExp e1 env, evalExp e2 env with
  | Int i1, Int i2 -> Bool (i1 = i2)
  | Bool b1, Bool b2 -> Bool (b1 = b2)
  | _ -> raise UndefinedSemantics

and evalInequality e1 e2 env =
  match evalExp e1 env, evalExp e2 env with
  | Int i1, Int i2 -> Bool (i1 <> i2)
  | Bool b1, Bool b2 -> Bool (b1 <> b2)
  | _ -> raise UndefinedSemantics

and evalConditional e1 e2 e3 env =
  match evalExp e1 env with
  | Bool true -> evalExp e2 env
  | Bool false -> evalExp e3 env
  | _ -> raise UndefinedSemantics

and evalLet v e1 e2 env =
  let value = evalExp e1 env
  let newEnv = Map.add v value env
  evalExp e2 newEnv

and evalLetFun f x e1 e2 env =
  let func = Func(x, e1, env)
  let newEnv = Map.add f func env
  evalExp e2 newEnv

and evalLetRec f x e1 e2 env =
  let recFunc = RecFunc(f, x, e1, env)
  let newEnv = Map.add f recFunc env
  evalExp e2 newEnv

// The program starts execution with an empty environment. Do not fix this code.
let run (prog: Program) : Val =
  evalExp prog Map.empty
