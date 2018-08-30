gen.perms<-function (n = 5) 
{
    all <- expand.grid(p1 = 1:n, p2 = 1:n, p3 = 1:n, stringsAsFactors = FALSE)
    perms <- all[apply(all, 1, function(x) {
        length(unique(x)) == 3
    }), ]
    perms
}
