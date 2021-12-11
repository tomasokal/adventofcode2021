string = '{([(<{}[<>[]}>{[]{[(<()>'
string = '[[<[([]))<([[{}[[()]]]'
string = '[{[{({}]{}}([{[{{{}}([]'
string = '<{([([[(<>()){}]>(<<{{'
string = '{[(<{[]}>)]}'
{[(<{[]}>)]}

isBalanced = function(x) {

    oldString = x
    while(TRUE){
        
        ss = oldString 
        ss = gsub("\\(\\)", "", ss) 
        ss = gsub("\\{\\}", "", ss) 
        ss = gsub("\\[\\]", "", ss) 
        ss = gsub("<>", "", ss)

        if(ss == oldString) {
            if(ss == ""){
                return("Balanced")
            } else {
                return(ss)
            }
        } else {
            print(ss)
            oldString = ss
        }
    }

}

isBalanced(string)

gsub("[()]", "", string)
gsub("[[]]", "", string)
gsub("[{}]", "", string)


