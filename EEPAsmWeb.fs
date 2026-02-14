
// ================================================================================
// Derived from works by Tom Clarke.
// Link of the original repository: https://github.com/tomcl/eepAssembler
// Modified by Lucas Luo in 2026 to support online assembing for EEP1 CPU
// ================================================================================
// Copyright (c) 2022 Tom Clarke
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
// ================================================================================

module EEPAsmWeb
open System

module EEExtensions =
    /// CHANGELIST
    /// July 2018: add String.regexMatchGroups, correct documentation for regexMatch

    /// Miscellaneous extensions to core F# library functions
    /// Additions to Char, String, Map


    /// various functions that exist in normal F# but cannot work in fable
    module FableReplacements =
        let optionMap2 f v1 v2 = match v1, v2 with
                                 | Some v1, Some v2 -> Some (f v1 v2)
                                 | _ -> None

        let listChunkBySize chunkSize l =
            let rec listChunkBySize' state chunksLeft itemsRemaining =
                match chunksLeft, itemsRemaining with
                | _, [] -> state
                | 0, _ -> listChunkBySize' ([] :: state) chunkSize itemsRemaining
                | _, nextItem::itemsTail -> listChunkBySize' ((nextItem :: List.head state) :: (List.tail state)) (chunksLeft - 1) itemsTail

            match l with
            | [] -> []
            | _ -> listChunkBySize' [] 0 l |> List.map List.rev |> List.rev

        let hexToString (x : uint32) =
            let rec loop str =
                function
                | 0u -> str
                | num -> loop ((sprintf "%X" (num % 16u)) + str) (num / 16u)
            match x with
            | 0u -> "0"
            | _ -> loop "" x

    // Following char method to convert to integer is partly based on code found on
    // http://www.fssnip.net/25/title/Hex-encode-decode
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Char =

        [<CompiledName("ToInt")>]
        let inline toInt(ch: char): int = 
            match ch with
            | c when c >= '0' && c <= '9' -> int c - int '0' 
            | c when c >= 'a' && c <= 'f' -> (int c - int 'a') + 10
            | c when c >= 'A' && c <= 'F' -> (int c - int 'A') + 10
            | _ -> failwithf "What ? Error while converting character to digit"

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module String =

        open System

        [<CompiledName("OfChar")>]
        let inline ofChar( ch: char): string = [| ch |] |> Seq.ofArray |> System.String.Concat

        [<CompiledName("OfSeq")>]
        let inline ofSeq (chars: char seq) : string = System.String.Concat chars

        [<CompiledName("ToSeq")>]
        let inline toSeq (str: string) : char seq = str :> char seq

        [<CompiledName("OfList")>]
        let inline ofList (chars: char list) =  chars |> Seq.ofList |> System.String.Concat


        [<CompiledName("OfArray")>]
        let inline ofArray (chars: char array) = chars |> Seq.ofArray |> System.String.Concat

        [<CompiledName("ToList")>]
        let inline toList (str: string): char list = str |> List.ofSeq

        [<CompiledName("ToArray")>]
        let inline toArray (str: string): char array = str |> Array.ofSeq

        /// splits text into its array of non-whitepace strings separated by whitespace
        [<CompiledName("SplitOnWhitespace")>]
        let splitOnWhitespace (text:string): string array = 
            text.Split( ([||]: char array) , System.StringSplitOptions.RemoveEmptyEntries)

        let [<Literal>] DefaultComparison = StringComparison.Ordinal
        let inline emptyIfNull str = 
            match str with
            | null -> String.Empty
            | _ -> str
        /// Concatenate a sequence of strings
        /// Using sep as separator
        [<CompiledName("Concat")>]
        let concat sep (strings : seq<string>) =  
            String.Join(sep, strings)

        [<CompiledName("Length")>]
        let length (str:string) =
            let str = emptyIfNull str
            str.Length

        /// True if str contains value
        [<CompiledName("Contains")>]
        let contains (value:string) (str:string) =
            str.Contains(value)

        [<CompiledName("Compare")>]
        let compare (strB:string) (strA:string) =
            String.Compare(strA, strB, DefaultComparison)

        /// True if str ends with value
        [<CompiledName("EndsWith")>]
        let endsWith (value:string) (str:string) =
            str.EndsWith(value, DefaultComparison)
        /// See String.Equals
        [<CompiledName("Equals")>]
        let equals (comparisonType:StringComparison) (value:string) (str:string) =
            str.Equals(value, comparisonType)

        let inline checkIndex func (comparisonType:StringComparison) value =
            let index = func(value, comparisonType)
            if index = -1 then None
            else Some index

        /// Replace all occurences of oldChar by newchar
        [<CompiledName("ReplaceChar")>]
        let replaceChar (oldChar:char) (newChar:char) (str:string) =
            str.Replace(oldChar, newChar)

        /// Replace all occurences of oldValue by newValue
        [<CompiledName("Replace")>]
        let replace (oldValue:string) (newValue:string) (str:string) =
            str.Replace(oldValue, newValue)

        /// Split str at all of separator array elements
        /// Return array of strings
        /// Adjacent separators generate empty strings
        [<CompiledName("Split")>]
        let split (separator:char array) (str:string) =
            str.Split(separator, StringSplitOptions.None)

        /// Split str at all of separator array elements
        /// Return array of strings
        /// Adjacent separators do not generate strings   
        [<CompiledName("SplitRemoveEmptyEntries")>]
        let splitRemoveEmptyEntries (separator:char array) (str:string) =
            str.Split(separator, StringSplitOptions.RemoveEmptyEntries)

        /// Split str at all of separator string array elements
        /// Return array of strings
        /// Adjacent separators generate empty strings
        [<CompiledName("SplitString")>]
        let splitString (separator:string array) (str:string) =
            str.Split(separator, StringSplitOptions.None)
        /// Split str at all of separator string array elements
        /// Return array of strings
        /// Adjacent separators do not generate strings
        [<CompiledName("SplitStringRemoveEmptyEntries")>]
        let splitStringRemoveEmptyEntries (separator:string array) (str:string) =
            str.Split(separator, StringSplitOptions.RemoveEmptyEntries)

        /// Return true if str starts with value
        [<CompiledName("StartsWith")>]
        let startsWith (value:string) (str:string) = 
            str.StartsWith(value, DefaultComparison)

        /// Return substring of str at startIndex of length chars
        /// Throw ArgumentOutOfRange exception if any part of
        /// selected string lies outside str.
        [<CompiledName("SubstringLength")>]
        let substringLength (startIndex:int) (length: int) (str:string) =
            str.Substring(startIndex, length)
        /// Return str from startIndex till end
        /// Throw ArgumentOutOfRange exception if startWith
        /// lies outside str
        [<CompiledName("Substring")>]
        let substring (startIndex:int) (str:string) =
            str.Substring(startIndex)

        [<CompiledName("ToLower")>]
        let toLower(str:string) =
            str.ToLowerInvariant()

        [<CompiledName("ToUpper")>]
        let toUpper(str:string) =
            str.ToUpperInvariant()
        /// Remove all leading and training whitespace
        [<CompiledName("Trim")>]
        let trim(str:string) =
            str.Trim()
        /// Remove all leading and trailing chars in trimChars
        [<CompiledName("TrimChars")>]
        let trimChars (trimChars:char []) (str:string) =
            str.Trim(trimChars)
        /// Remove all leading whitespace
        [<CompiledName("TrimStart")>]
        let trimStart (trimChars:char []) (str:string) =
            str.TrimStart(trimChars)
        /// Remove all trailing whitespace    
        [<CompiledName("TrimEnd")>]
        let trimEnd(trimChars:char []) (str:string) =
            str.TrimEnd(trimChars)

        /// Match a regular expression
        /// Return Some [grps] where m is the match string,
        /// grps is the list of match groups (if any)
        /// return None on no match
        [<CompiledName("RegexMatchGroups")>]
        let regexMatchGroups (regex:string) (str:string) =
            let m = Text.RegularExpressions.Regex.Match(str, regex)
            if m.Success then 
                Some [ for n in [1..m.Groups.Count] -> m.Groups[n].Value ]
            else None

        /// Match a regular expression
        /// Return Some m where m is the match string,
        /// return None on no match
        [<CompiledName("RegexMatch")>]
        let regexMatch (regex:string) (str:string) =
            let m = Text.RegularExpressions.Regex(regex).Match(str)
            if m.Success
            then
                Some m.Value // TODO workaround
                //let mLst = [ for x in m.Groups -> x.Value ]
                //Some (List.head mLst, List.tail mLst)
            else None

        /// convert a System.XXX numeric parse function to idiomatic F# option.
        /// e.g. String.TryParsewith System.Int32 will return Some n on successful Int32 parse or None.
        [<CompiledName("TryParseWith")>]
        let tryParseWith (tryParseFunc: string -> bool*'T) = tryParseFunc >> function
            | true, v    -> Some v
            | false, _   -> None


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module List =
        [<CompiledName("ToString")>]
        let toString (chars: char list) =  chars |> Seq.ofList |> System.String.Concat

        /// Split list into list of lists each such that each element for which pred returns true starts a sublist.
        /// Every sublist must contain at least one element.
        /// Every sublist except possibly the first starts with an element el for which pred el is true
        [<CompiledName("ChunkAt1")>]
        let chunkAt1 pred lst = 
            let mutable i = 0 // should optimise this using sequences and yield! to group by subarray
            [ for el in lst do
                if pred el then i <- i + 1
                yield (i, el);
              yield! []]
            |> List.groupBy fst
            |> List.sortBy fst
            |> List.map (snd >> (List.map snd))

        /// Split list into list of lists each such that each element for which pred returns true starts a sublist.
        /// Every sublist must contain at least one element.
        /// Every sublist except possibly the first starts with an element el for which pred el is true.    
        [<CompiledName("ChunkAt")>]
        let chunkAt pred list = 
          let rec loop chunk chunks list = 
            match list with
            | [] -> List.rev ((List.rev chunk)::chunks)
            | x::xs when pred x && List.isEmpty chunk -> loop [x] chunks xs
            | x::xs when pred x -> loop [x] ((List.rev chunk)::chunks) xs
            | x::xs -> loop (x::chunk) chunks xs
          loop [] [] list


        /// Extract Ok elements from result list, return list of Ok values







        [<CompiledName("OkList")>]
        let okList lst = [ for x in lst do match x with | Ok y -> yield y | _ -> (); yield! []]

        /// Extract Error elements from result list, return list of errors
        [<CompiledName("ErrorList")>]
        let errorList lst = [ for x in lst do match x with | Error y -> yield y | _ -> (); yield! []]

        /// split Result list into pair of Ok and Error value lists repectively
        [<CompiledName("SplitResult")>]
        let splitResult resL =
            List.fold (fun (rl,el) -> function | Error e -> rl, e :: el | Ok r -> r :: rl, el) ([],[]) resL


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Array =

        [<CompiledName("ToString")>]
        let toString (chars: char array) = chars |> Seq.ofArray |> System.String.Concat

        /// Split array into array of arrays each such that each element for which pred returns true starts a subarray.
        /// Every subarray must contain at least one element.
        /// Every subarray except possibly the first starts with an element el for which pred el is true.
        [<CompiledName("ChunkAt")>]
        let chunkAt pred arr = // should optimise this using sequences and yield! to group by subarray
            let mutable i = 0
            [| for x in arr do
                if pred x then i <- i + 1
                yield i, x |]
            |> Array.groupBy fst
            |> Array.map (snd >> (Array.map snd))



    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Map =
        /// Looks up key in table, returning defaultValue if 
        /// key is not in table
        [<CompiledName("FindWithDefault")>]
        let findWithDefault (key:'Key) (table:Map<'Key,'Value>) (defaultValue:'Value) =
            match table.TryFind key with | Some v -> v |None -> defaultValue

        /// Return array of all values in table
        [<CompiledName("Values")>]
        let values (table:Map<'Key,'Value>) =
            table |> Map.toArray |> Array.map snd

        /// Return array of all keys in table
        [<CompiledName("Keys")>]
        let keys (table:Map<'Key,'Value>) =
            table |> Map.toArray |> Array.map fst




open EEExtensions

type Register = Regist of int | Flags | PCX

type Phase = | Phase1 | Phase2

let version = "2.3"

type Op = 
    | Imm4 of int
    | Imm5 of int
    | Imm8 of int 
    | RegOp of Register
    | OffsetOp of Register * int
    | SymImm8 of string

type SymTable = 
    { Table: Map<string,int>; Phase : Phase}

        member this.Lookup name =
            match this.Phase, Map.tryFind name this.Table with
            | _, Some n -> 
                Ok n
            | Phase1, None -> 
                Ok 0
            | Phase2, None   -> 
                Error $"Can't find symbol '{name}' in symbol table"

        member this.AddSymbol name value =
            if Map.containsKey name this.Table then
                Error $"Duplicate definition of symbol '{name}'"
            else
                Ok {this with Table = Map.add name value this.Table }

        static member Initial = {Table = Map.empty; Phase = Phase1}
    

type IWord = 
    {
        Dat: uint32}
        static member JmpCode = uint32 0xC000
        static member ExtCode = uint32 0xD000
        static member AluOpcField n = uint32 (n <<< 12)
        static member JmpOpcField n = uint32 (n <<< 9)
        static member JmpInvBit b = uint32 <| if b then (1 <<< 8) else 0
        static member Imm8Bit b = uint32 <| if b then (1 <<< 8) else 0
        static member RaField n = uint32 (n <<< 9)
        static member RbField n = uint32 (n <<< 5)
        static member RcField n = uint32 (n <<< 2)
        static member ShiftOpcField n = uint32 (((n &&& 1) <<< 4) + ((n &&& 2) <<< 8 - 1))
        static member Rb b = IWord.RbField b
        static member Imm8Field n = uint32 ( n &&& 0xFF)
        static member MemOp n = uint32 0x8000u + (uint32 n <<< 12)
        static member ExtOp n = 0xD000u + uint32 n
        static member IsThreeRegOp n = 
            match n with
            | 0 | 7 | 6 -> false
            | _ -> true
        static member makeMOVC(c, a, b) =
            IWord.RcField c + IWord.RaField a + IWord.RbField b

type Token = 
    | ALUOP of int 
    | MOVEXTRA of int
    | SHIFTOP of int
    | JMPOP of int * bool 
    | MEMOP of int
    | EXTOP
    | DCW
    | ORG
    | RETINT
    | SETI
    | CLRI
    | Imm of int 
    | Reg of Register
    | Symbol of string
    | Hash
    | LBra
    | RBra
    | ErrorTok of string
    | Comment of string



type Line = 
    {
        Label: string option
        Word: Result<uint32,string> option
        LineNo: int
        Address: uint32
        Table: SymTable
        Phase: Phase
        Comment: string
        ExtMod: uint32 option
    }
        static member First =
            {
                Label=None
                Word = None
                LineNo = 1
                Address = 0u
                Table = SymTable.Initial
                Phase = Phase1
                Comment = ""
                ExtMod = None
            }





let opMap: Map<string,Token> = 
    Map.ofList [
        "R0", Reg (Regist 0)
        "R1", Reg (Regist 1)
        "R2", Reg (Regist 2)
        "R3", Reg (Regist 3)
        "R4", Reg (Regist 4)
        "R5", Reg (Regist 5)
        "R6", Reg (Regist 6)
        "R7", Reg (Regist 7)
        "MOV", ALUOP 0
        "MOVC1", MOVEXTRA 1
        "MOVC2", MOVEXTRA 2
        "MOVC3", MOVEXTRA 3
        "MOVC4", MOVEXTRA 4
        "MOVC5", MOVEXTRA 5
        "MOVC6", MOVEXTRA 6
        "MOVC7", MOVEXTRA 7
        "ADD", ALUOP 1
        "SUB", ALUOP 2
        "ADC", ALUOP 3
        "SBC", ALUOP 4
        "AND", ALUOP 5
        "CMP", ALUOP 6
        "LSL", SHIFTOP 0
        "LSR", SHIFTOP 1
        "ASR", SHIFTOP 2
        "XSR", SHIFTOP 3
        "LDR", MEMOP 0
        "STR", MEMOP 2
        "JMP", JMPOP (0,false)
        "EXT", EXTOP
        "JNE", JMPOP (1,true)
        "JEQ", JMPOP (1,false)
        "JCS", JMPOP (2,false)
        "JCC", JMPOP (2,true)
        "JMI", JMPOP (3,false)
        "JPL", JMPOP (3,true)
        "JGE", JMPOP (4,false)
        "JLT", JMPOP (4,true)
        "JGT", JMPOP (5,false)
        "JLE", JMPOP (5,true)
        "JHI", JMPOP (6,false)
        "JLS", JMPOP (6,true)
        "JSR", JMPOP (7,false)
        "RET", JMPOP (7,true)
        "#", Hash
        "[", LBra
        "]", RBra
        "DCW", DCW
        "ORG", ORG
        "RETINT", RETINT
        "SETI", SETI
        "CLRI", CLRI
        "FLAGS", Reg Flags
        "PCX", Reg PCX
        ]


let toksToString tokL =
    let tokToS (tok:Token) =
        match tok, Map.tryPick (fun key value -> if value = tok then Some key else None) opMap with
        | _, Some key-> key
        | Symbol s, _ -> s
        | Comment "",_ -> ""
        | Comment comment, _-> "// " + comment
        | Imm n,_ -> $"#{n}"
        | _ -> sprintf "'%A'" tok
    tokL
    |> List.map tokToS
    |> String.concat  " "

/// split the input line, remove white space, return list of strings as tokens
let tokenize (s:string) =
    let s' = s.Replace("#"," # ").Replace(","," , ").Replace("["," [ ").Replace("]"," ] ")
    let sL =
        s'.Split([|"//"|],StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
    let s, comment =
        match sL with
        | [] -> "", Comment ""
        | [commentText] when String.startsWith "//" s' ->
            "", Comment commentText 
            
        | line :: comment -> line, Comment ((String.concat "//" comment).Trim())
    s.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList
    |> List.map (fun s -> s.ToUpper())
    |> List.filter (fun s -> s <> "," && s <> "")
    |> List.map (fun s -> 
        match Map.tryFind s opMap with
        | Some tok -> tok
        | None when Seq.contains (s.Chars 0) "0123456789-" ->
            try Some(int32 s) with | e -> None
            |> (function | None -> ErrorTok $"can't parse {s} as an integer"                 
                         | Some n -> Imm n)
        | None when (Char.IsLetter (s.Chars 0))-> Symbol s
        | None -> ErrorTok $"'{s}' is not recognised")
    |> (fun toks -> toks @ [comment])

let checkReg ra =
    match ra with
    | Regist a-> Ok a
    | ra -> Error $"{ra} is not allowed as an operand in this instruction"

let check2Regs ra rb =
    checkReg ra
    |> Result.bind (fun a -> 
        checkReg rb
        |> Result.map (fun b -> (a,b)))

let check3Regs ra rb rc =
    check2Regs ra rb
    |> Result.bind (fun (a,b) -> 
        checkReg rc
        |> Result.map (fun c -> (a,b,c)))

let makeOp isJmp (pc:int) (a: int) (op:Op) =            
    fun (symTab:SymTable) ->
        let ra =  IWord.RaField a
        match op with
        |  Imm4 n ->
            Ok (uint32 n)  
        |  OffsetOp(Regist rb,n) ->
            Ok <| ra + IWord.RbField rb + IWord.Imm8Field (n &&& 0x1F) 
        | OffsetOp(rb,_) ->
           Error $"{rb} is not allowed as an operand in this instruction"
        |  Imm8 n when n <= 255 && n >= -128 || n <= 65535 && n >= 65536 - 128 -> 
            Ok <| ra + IWord.Imm8Field n + IWord.Imm8Bit (not isJmp)
        |  Imm8 n -> Error $"Immediate operand {n} is not in the allowed range 255 .. -128"
        |  SymImm8 s when not isJmp->
            symTab.Lookup s
            |> Result.map (fun n -> ra + IWord.Imm8Field n + IWord.Imm8Bit (not isJmp))
        | SymImm8 s when symTab.Phase = Phase1 -> // if isJump
            Ok (ra) // can't error in Phase1 to allow forward references
        |  SymImm8 s -> // if isJmp
            symTab.Lookup s
            |> Result.bind (fun n -> 
                let offset = n - pc
                if offset < -128 || offset > 127 
                then Error $"Operand for jump is outside allowed range -128 - +127: target=%0x{n}, pc=%0x{pc}"
                else Ok (ra + IWord.Imm8Field offset))
        |  RegOp (r) when isJmp ->
            Error $"Jump instruction is not allowed register operand '{r}'"
        |  RegOp(Regist r) ->
            Ok <| ra + IWord.Rb r
        | RegOp r ->
            Error $"{r} is not allowed as an operand in this instruction"           
        | Imm5 _ -> failwithf "What? Imm5 is not allowed in makeOP"


let makeAluOp ra n op =
    fun symTab ->
        checkReg ra
        |> Result.bind (fun a -> makeOp false 0 a op symTab)
        |> Result.map (fun w -> w + IWord.AluOpcField n)


let makeMovExtraOp ra rb x =
    fun symTab ->
        check2Regs ra rb
        |> Result.bind (fun (a,b) -> 
            Ok (IWord.RaField a ||| IWord.RbField b ||| IWord.RcField x))

let makeAluOp3 n ra rb rc =    
    fun _  ->
        check3Regs ra rb rc
        |> Result.bind (fun (a,b,c) ->
            if IWord.IsThreeRegOp n then
                Ok (IWord.RaField a ||| IWord.RbField b ||| IWord.RcField c ||| IWord.AluOpcField n)
            else
                Error $"ALU op '{n}' does not support 3 register operands")
        

let makeMemOp ra n op =
    fun symTab ->
            makeOp false 0 ra op symTab
            |> Result.map (fun w -> w + IWord.MemOp n)




let makeJmpOp (inv:bool) (pc:int) ra (n:int) op =
    fun symTab ->
        makeOp true pc ra op symTab
        |> Result.map (fun w -> 
            (w &&& 255u) + IWord.JmpOpcField n + IWord.JmpInvBit inv + IWord.JmpCode)

let makeShiftOp rb ra n x =
    fun _ ->
        check2Regs ra rb
        |> Result.bind (fun (a,b) ->
            match x with
            | Imm4 sCnt -> 
                Ok (IWord.RaField a ||| IWord.RbField b ||| IWord.AluOpcField 7 ||| IWord.ShiftOpcField n ||| IWord.Imm8Field sCnt)
            | _ ->
                Error $"ALU op '{n}' is not a shift op")
        
            
let (|ParseOpInner|_|) toks =
    match toks with
    | Symbol s :: rest -> 
        Some (Ok (SymImm8 s), rest)
    | Hash :: Imm n :: rest | Imm n :: rest -> 
        Some (Ok (Imm8 n), rest)
    | Reg r :: rest  -> 
        Some (Ok (RegOp r), rest)
    | toks -> Some (Error $"'{toksToString toks}' found when operand expected",[]) // nothing else matches.

let (|ParseComment|_|) comments =
    match comments with
    | [ Comment c ] -> Some (Ok c)
    | toks -> Some (Error $"'{toksToString toks}' found when comment or end-of-line expected")

let makeParse extMod (op:Result<Op,string>) (comment: Result<string,string>) =
    let addExtMod op =
        match extMod, op with   
        | None, x -> Ok x
        | Some _, SymImm8 _ -> Error "EXT cannot be used to modify a symbol - replace the symbol by a literal"
        | _, x -> Ok x
    match op,comment with
    | Error op, _ -> Some (Error op,"")
    | _, Error comment -> Some (Error comment,"")
    | Ok op, Ok comment -> Some (addExtMod op,comment)

let (|Imm4Inner|_|) tok =
    match tok with
    | Imm n when n >= 0 && n < 16 ->
        Some (Ok (Imm4 n))
    | Imm n -> 
        Some (Error $"'{toksToString [tok]}' found when shift count in range 0 to 15 expected")
    | _ -> 
        None

let (|Imm5Inner|_|) tok =
    match tok with
    | Imm n when n >= -16 && n < 16 ->
        Some (Ok (Imm5 n))
    | Imm n -> 
        Some (Error $"'{toksToString [tok]}' found when shift count in range 0 to 15 expected")
    | _ -> 
        None


let (|ParseImm4|_|) toks =
    match toks with
    | Hash :: Imm4Inner n :: ParseComment c | Imm4Inner n :: ParseComment c -> 
        makeParse None n c
    | toks -> Some (Error $"'{toksToString toks}' found when integer Shift Count expected","") // nothing else matches.

let (|ParseOffsetMemOp|_|) extMod toks =
    match toks with
    | LBra:: Reg n :: Hash :: Imm5Inner imm :: RBra :: ParseComment c 
    | LBra :: Reg n :: Imm5Inner imm :: RBra :: ParseComment c -> 
                let getImm4Op = function
                    | Ok (Imm5 imm) -> Ok <| OffsetOp(n,imm)
                    | Ok x -> Error $"What? expecting Imm5, {x} should not happen"
                    | Error s -> Error s
                makeParse extMod (getImm4Op imm) c
    | _ -> None
        

let (|ParseOp|_|) extMod useBrackets toks =
    match useBrackets, toks with
    | true, LBra :: ParseOpInner (op,RBra :: ParseComment c) -> 
        makeParse extMod op c
    | _, ParseOpInner( op, ParseComment c) -> 
        makeParse extMod op c
    | _ -> 
        failwithf $"What? Can't parse {(useBrackets, toks)}"

// Parses, a tokenised line of text
let rec parseUnlabelled (line: Line) (tokL: Token list) : Line =
    let line = {line with Table = {line.Table with Phase = line.Phase}}
    //printfn $"Parsing {line.Phase} {line.Table.Phase} {line.Address}:'{toksToString tokL}'"
    let nl = {line with LineNo = line.LineNo + 1; ExtMod = None}
    let wordOf1 (wordRes: SymTable -> Result<uint32,string>) (c: Result<string,string>)  =
        match wordRes line.Table, c with
        | Ok res, Ok comment -> 
            {nl with Word = Some (Ok res); Comment = comment}
        | _, Error c ->
            {nl with Word = Some (Error c)}
        | Error s, _->
            {nl with Word = Some (Error s)}

    let wordOf wordGen ra n op =
        match op with
        | Ok op', comment -> 
            {nl with Word = Some (wordGen ra n op' line.Table); Comment = comment}
        | Error s, _->
            {nl with Word = Some (Error s)}
    let makeMovcInstruction c a b s = 
        {nl with Word = Some (Ok <| IWord.makeMOVC(c,a,b))}, [Comment s]
    let (|ParseOpWithExt|_|) = (|ParseOp|_|) line.ExtMod
    let (|ParseOffsetMemOpWithExt|_|) = (|ParseOffsetMemOp|_|) line.ExtMod
    let lineError s = {nl with Word = Some <| Error s}
    let error = List.tryPick (function | ErrorTok s -> Some s | _ -> None) tokL
    let tokString = toksToString tokL
    match error, tokL with
    | Some s, rest -> 
        lineError $"Line {line.LineNo}: Token error: '{s}'", rest
    | _, [] -> 
        {nl with Word = None}, []
    |_, [Comment s] ->
        {nl with Word = None}, [Comment s]
    | _, [RETINT; Comment s] -> makeMovcInstruction 6 0 0 s
    | _, [SETI; Comment s] -> makeMovcInstruction 6 0 1 s
    | _, [CLRI; Comment s] -> makeMovcInstruction 6 0 2 s
    | _, [ALUOP 0; Reg Flags; Reg (Regist a); Comment s ] -> 
        makeMovcInstruction 7 a 0 s
    | _, [ALUOP 0; Reg PCX; Reg (Regist a); Comment s ] -> 
        makeMovcInstruction 7 a 1 s
    | _, [ALUOP 0; Reg (Regist a); Reg Flags; Comment s ] -> 
        makeMovcInstruction 7 a 2 s
    | _, [ALUOP 0; Reg (Regist a); Reg PCX;  Comment s ] -> 
        makeMovcInstruction 7 a 3 s
    |_, SHIFTOP s :: Reg a :: Reg b :: ParseImm4 (op) ->
        wordOf (makeShiftOp b) a s op, []
    | _, [ EXTOP ; Imm n ; Comment s ]
    | _, [ EXTOP ; Hash; Imm n ; Comment s ] ->
        if n >= 0 then 
            {nl with Word = Some (Ok (IWord.ExtCode + uint32 n)); ExtMod = Some (uint32 n &&& 0xFFu)}, [Comment s]
        else 
            {nl with Word = Some (Error "EXT must have number in range 0 .. 0xFF")}, [Comment s]
    | _, ALUOP n :: Reg rc :: Reg ra :: Reg rb :: ParseComment c ->
        wordOf1 (makeAluOp3 n ra rb rc) c, []
    | _, ALUOP n :: Reg ra :: Reg rb :: ParseComment c when n <> 0 && n <> 6 ->
        wordOf1 (makeAluOp3 n ra rb ra) c, []
    | _, ALUOP n :: Reg ra :: ParseOpWithExt false (op) ->
        wordOf makeAluOp ra n op,[]
    | _, MOVEXTRA n :: Reg ra :: Reg rb :: ParseComment c ->
        wordOf1 (makeMovExtraOp ra rb n) c, []
    | _, [JMPOP (7,true) ; Comment s] -> // special case for RET
        {nl with Word = Some (Ok (uint32 (IWord.JmpCode + IWord.JmpOpcField 7 + IWord.JmpInvBit true)))}, [Comment s]
    | _, JMPOP (n1,inv) :: ParseOpWithExt false (op) ->
        wordOf (makeJmpOp inv (int nl.Address)) 0 n1 op, []
    | _, MEMOP n :: Reg (Regist a) :: ParseOffsetMemOpWithExt op
    | _, MEMOP n :: Reg (Regist a) :: ParseOpWithExt true (op) ->
        wordOf makeMemOp a n op, []
    | _, [ DCW ; Imm n ; Comment s]
    | _, [ DCW ; Hash; Imm n ; Comment s] ->
        {nl with Word = Some (Ok (uint32 n))}, [Comment s]
    | _, [ORG ; Imm n;  Comment s]
    | _, [ORG ; Hash; Imm n;  Comment s] ->
        {nl with Address = uint32 n; Word = None}, [Comment s]
        
    | _ when line.Label.IsSome ->
        lineError $"Error in '{tokString}' after symbol '{line.Label.Value}', \
                    perhaps this is a mis-spelled opcode mnemonic?",[]
    | _  -> 
        lineError $"Unexpected parse error: '{tokString}'"   , []
    |> (fun (line, rest) -> 
        match line, rest with
        | {Word = Some (Error msg)}, _ -> 
            line
        | _, [Comment comment] ->
            {line with Comment = comment}
        | _, [] ->
            {line with Comment = ""}
        | _, rest -> 
            {line with Word = Some (Error (toksToString rest))})
    |> (fun line' -> 
            let usesMemory = line'.Word <> None
            //printfn $"line:{line'.LineNo}, address:{line'.Address} toks={tokString}, usesMemory={usesMemory}"
            {line' with Address = line'.Address + if usesMemory then 1u else 0u})

let parse (line: Line) (tokL: Token list) : Line =
    let lineError s = {line with Word = Some <| Error s; LineNo = line.LineNo + 1}
    match tokL with
    | Symbol s :: tokL' ->
         let table = 
             match line.Phase with
             | Phase1 -> line.Table.AddSymbol s (int line.Address)
             | Phase2 -> Ok line.Table
         match table with
         |  Error _ -> 
             lineError  $"Duplicate label: '{s}'"
         | Ok table' ->
             parseUnlabelled {line with Table = table'; Label = Some s} tokL'
    | _ ->
        parseUnlabelled {line with Label = None} tokL



// runs the assembler REPL (removed in web version)



let parseLines (txtL: string list) =
    let incAddress (line:Line) = {line with Address = line.Address + 1u}
    let errorLine (line:Line) (msg:string) =
        $"Line no {line.LineNo - 1}: %s{msg}"

    let getErrors (lines: Line list) =
        lines
        |> List.collect (function | {Word=Some (Error msg )} as line -> 
                                        [errorLine line msg]  
                                  | _ -> [])

    let parseFolder (line,outs) tokL =
            let line = {parse line tokL with Label = None}
            line, outs @ [line]

    let tokLines = 
        txtL
        |> List.map tokenize

    let firstPass =
        let folder line toks =
            parse {line with Label = None} toks
        (Line.First, tokLines)
        ||> List.scan folder



    match getErrors firstPass with
    | [] -> 
        let init = {Line.First with 
                        Table = (List.last firstPass).Table; 
                        Address = 0u
                        Phase = Phase2}
        ((init,[]), tokLines)
        ||> List.fold parseFolder
        |> (fun (line, outs) ->
            match getErrors outs with
            | [] ->  
                outs
                |> List.filter (fun line -> line.Word <> None)
                |> Ok
            | lst -> Error lst)
    | lst -> Error lst

let formatAssembly (lines: Line list) =
    lines
    |> List.map (fun line ->
        let num = line.Address
        let word = match line.Word with | Some (Ok n) -> n | _ -> 0u
        sprintf "%s" $"0x%02x{num-1u} 0x%04x{word}")





// =====================================================
// Web api, REPL are removed in web version
// =====================================================

/// Assemble from a list of source lines
let assembleLines (lines: string list) : Result<string, string list> =
    parseLines lines
    |> Result.map (fun assembled ->
        formatAssembly assembled |> String.concat "\n")

/// Assemble from a single source string, returning a single formatted error string
let assembleText (src: string) : Result<string, string> =
    let normalised =
        src.Replace("\r\n", "\n").Replace("\r", "\n")
    
    let lines =
        normalised.Split('\n') |> Array.toList
    
    assembleLines lines
    |> Result.mapError (fun errs -> String.concat "\n" errs)
