orient.phaseII.1<-function (cgraph, nvars) 
{
    if (nvars < 3) 
        return(cgraph)
    var.trips <- combn(1:nvars, 3)
    ntrips <- dim(var.trips)[2]
    all.triplets <- expand.grid(p1 = 1:3, p2 = 1:3, p3 = 1:3, 
        stringsAsFactors = FALSE)
    triplet.perms <- all.triplets[apply(all.triplets, 1, function(x) {
        length(unique(x)) == 3
    }), ]
    n.triplet.perms <- dim(triplet.perms)[1]
    if (nvars > 3) {
        var.quads <- combn(1:nvars, 4)
        nquads <- dim(var.quads)[2]
        all.quads <- expand.grid(p1 = 1:4, p2 = 1:4, p3 = 1:4, 
            p4 = 1:4, stringsAsFactors = FALSE)
        quad.perms <- all.quads[apply(all.quads, 1, function(x) {
            length(unique(x)) == 4
        }), ]
        n.quad.perms <- dim(quad.perms)[1]
    }
    old <- cgraph
    test <- 1
    while (test != 0) {
        for (i in 1:ntrips) {
            for (j in 1:n.triplet.perms) {
                x <- triplet.perms[j, 1]
                y <- triplet.perms[j, 2]
                z <- triplet.perms[j, 3]
                if (cgraph[var.trips[x, i], var.trips[y, i]] == 
                  2 & cgraph[var.trips[y, i], var.trips[z, i]] == 
                  1 & cgraph[var.trips[z, i], var.trips[y, i]] == 
                  1 & cgraph[var.trips[x, i], var.trips[z, i]] == 
                  0) 
                  cgraph[var.trips[y, i], var.trips[z, i]] <- 2
                if (cgraph[var.trips[x, i], var.trips[y, i]] == 
                  2 & cgraph[var.trips[y, i], var.trips[z, i]] == 
                  2 & cgraph[var.trips[x, i], var.trips[z, i]] == 
                  1 & cgraph[var.trips[z, i], var.trips[x, i]] == 
                  1) 
                  cgraph[var.trips[x, i], var.trips[z, i]] <- 2
            }
        }
        if (nvars < 4) 
            return(cgraph)
        for (i in 1:nquads) {
            for (j in 1:n.quad.perms) {
                w <- quad.perms[j, 1]
                x <- quad.perms[j, 2]
                y <- quad.perms[j, 3]
                z <- quad.perms[j, 4]
                if (cgraph[var.quads[w, i], var.quads[x, i]] == 
                  1 & cgraph[var.quads[x, i], var.quads[w, i]] == 
                  1 & cgraph[var.quads[w, i], var.quads[y, i]] == 
                  1 & cgraph[var.quads[y, i], var.quads[w, i]] == 
                  1 & cgraph[var.quads[w, i], var.quads[z, i]] == 
                  1 & cgraph[var.quads[z, i], var.quads[w, i]] == 
                  1 & cgraph[var.quads[x, i], var.quads[z, i]] == 
                  2 & cgraph[var.quads[z, i], var.quads[x, i]] == 
                  1 & cgraph[var.quads[y, i], var.quads[z, i]] == 
                  2 & cgraph[var.quads[z, i], var.quads[y, i]] == 
                  1) 
                  cgraph[var.quads[w, i], var.quads[z, i]] <- 2
                if (cgraph[var.quads[w, i], var.quads[x, i]] == 
                  1 & cgraph[var.quads[x, i], var.quads[w, i]] == 
                  1 & cgraph[var.quads[w, i], var.quads[y, i]] == 
                  1 & cgraph[var.quads[y, i], var.quads[w, i]] == 
                  1 & cgraph[var.quads[w, i], var.quads[z, i]] > 
                  0 & cgraph[var.quads[z, i], var.quads[w, i]] > 
                  0 & cgraph[var.quads[x, i], var.quads[z, i]] == 
                  1 & cgraph[var.quads[z, i], var.quads[x, i]] == 
                  2 & cgraph[var.quads[y, i], var.quads[z, i]] == 
                  2 & cgraph[var.quads[z, i], var.quads[y, i]] == 
                  1) 
                  cgraph[var.quads[w, i], var.quads[x, i]] <- 2
            }
            test <- sum(old != cgraph)
            if (test != 0) 
                old <- cgraph
        }
    }
    cgraph
}
