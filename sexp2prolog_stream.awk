#!/usr/bin/awk -f
# streaming S-exp -> Prolog style
# input protocol (one line per result):
#   nil                => false.
#   t                  => true.
#   (((var X) a) ...)  => X = a, ... .

function tokenize(s,    i,c,buf,instr,esc) {
    NT=0; buf=""; instr=0; esc=0
    for (i=1;i<=length(s);i++) {
        c=substr(s,i,1)
        if (instr) {
            buf = buf c
            if (esc) { esc=0; continue }
            if (c=="\\") { esc=1; continue }
            if (c=="\"") { TOK[++NT]=buf; buf=""; instr=0 }
            continue
        }
        if (c=="\"") {
            if (buf!="") { TOK[++NT]=buf; buf="" }
            buf="\""; instr=1; esc=0
            continue
        }
        if (c=="(" || c==")") {
            if (buf!="") { TOK[++NT]=buf; buf="" }
            TOK[++NT]=c
            continue
        }
        if (c ~ /[ \t\r\n]/) {
            if (buf!="") { TOK[++NT]=buf; buf="" }
            continue
        }
        buf = buf c
    }
    if (buf!="") TOK[++NT]=buf
}

function parse_sexp(    i,t,sp,id,pid) {
    N=0; sp=0; ROOT=0
    for (i=1;i<=NT;i++) {
        t = TOK[i]
        if (t=="(") {
            id=++N; T[id]="L"; LEN[id]=0
            ST[++sp]=id
        } else if (t==")") {
            id=ST[sp--]
            if (sp==0) ROOT=id
            else {
                pid=ST[sp]
                CHILD[pid, ++LEN[pid]] = id
            }
        } else {
            id=++N; T[id]="A"; A[id]=t
            if (sp==0) ROOT=id
            else {
                pid=ST[sp]
                CHILD[pid, ++LEN[pid]] = id
            }
        }
    }
    return ROOT
}

function is_atom(id){ return T[id]=="A" }
function is_list(id){ return T[id]=="L" }

function atom_text(id, v) {
    v=A[id]
    if (v=="nil" || v=="NIL" || v=="|[]|") return "[]"
    if (v=="t" || v=="T") return "true"
    if (v ~ /^"/) return v
    return v
}

function is_nil_atom_id(id) {
    return is_atom(id) && (A[id]=="nil" || A[id]=="NIL" || A[id]=="|[]|")
}

function is_dot_list_node(id,    n,head) {
    if (!is_list(id)) return 0
    n = LEN[id]
    if (n != 3) return 0
    head = CHILD[id,1]
    return is_atom(head) && (A[head]=="|.|" || A[head]==".")
}

function list_to_prolog(id,    cur,res,first,h) {
    cur = id
    res = "["
    first = 1
    while (is_dot_list_node(cur)) {
        h = CHILD[cur,2]
        if (!first) res = res ","
        res = res term_to_prolog(h)
        first = 0
        cur = CHILD[cur,3]
    }
    if (is_nil_atom_id(cur)) return res "]"
    if (first) return "[" "|" term_to_prolog(cur) "]"
    return res "|" term_to_prolog(cur) "]"
}

# term:
#  atom -> atom_text
#  (var X ...) -> X
#  (f a b ...) -> f(a,b,...)
function term_to_prolog(id,    n,head,fun,i,s) {
    if (is_atom(id)) return atom_text(id)
    if (is_dot_list_node(id)) return list_to_prolog(id)
    n = LEN[id]
    if (n==0) return "[]"
    head = CHILD[id,1]

    if (is_atom(head) && (A[head]=="var" || A[head]=="VAR")) {
        if (n>=2) return atom_text(CHILD[id,2])
        return "_"
    }

    fun = term_to_prolog(head)
    if (n==1) return fun
    s = fun "("
    for (i=2;i<=n;i++) {
        if (i>2) s = s ","
        s = s term_to_prolog(CHILD[id,i])
    }
    s = s ")"
    return s
}

# subst is list of bindings: ( ((var X) val) ... )
function print_subst(root,    nb,bi,bind,key,val,varname,line,has) {
    if (!is_list(root)) { print "false."; return }
    nb = LEN[root]
    if (nb==0) { print "true."; return }

    line=""; has=0
    for (bi=1; bi<=nb; bi++) {
        bind = CHILD[root,bi]
        if (!is_list(bind) || LEN[bind] < 2) continue
        key = CHILD[bind,1]
        val = CHILD[bind,2]

        if (is_list(key) && LEN[key] >= 2 && is_atom(CHILD[key,1]) && (A[CHILD[key,1]]=="var" || A[CHILD[key,1]]=="VAR")) {
            varname = atom_text(CHILD[key,2])
        } else {
            varname = term_to_prolog(key)
        }

        if (has) line = line ", "
        line = line varname " = " term_to_prolog(val)
        has=1
    }

    if (!has) print "true."
    else print line "."
}

{
    line=$0
    gsub(/^[ \t\r\n]+/, "", line)
    gsub(/[ \t\r\n]+$/, "", line)
    if (line=="") next

    if (line=="nil" || line=="NIL") { print "false."; next }
    if (line=="t" || line=="T")     { print "true.";  next }

    tokenize(line)
    split("", T); split("", A); split("", LEN); split("", CHILD); split("", ST)
    root=parse_sexp()
    print_subst(root)
}
