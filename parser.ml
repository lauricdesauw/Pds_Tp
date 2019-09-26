open ASD
open Token

let rec parse = parser 
    | [< L_CHEVRON; STR(x); R_CHEVRON; SEMICOLON >] -> Obj x (* return the cpl in obj version *)
    | [< QUOT; STR(x); QUOT; SEMICOLON >] -> Txt x (* return the cpl in txt version *)
    | [< >] -> (*cpl, _  -> we have to make a cpl list *)
    (*attr cpl; -> attr of cpl *)
    (*attr, _ -> we have to make the list of attr *)
    (*subject attr -> subject of attr *)
    ;

