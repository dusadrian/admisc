test_that("using agrees with independent evaluations across the stress suite", {
    packages <- c("tibble", "data.table", "MASS", "survival", "declared", "haven", "statistics")
    missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing_packages)) {
        stop("The full using() suite requires: ", paste(missing_packages, collapse = ", "))
    }
    set.seed(1901)
    d <- data.frame(x = c(1,2,4,5,1,3,5,8), y = c(2,5,8,11,3,10,16,23),
                    g = factor(rep(c("A","B"), each=4)), z = rep(c("L","H"), 4))
    groups <- split(d, d$g)
    L <- as.list(d)
    M <- as.matrix(d[c("x","y")])
    E <- list2env(L)
    x <- 777
    nm <- "x"
    formula0 <- y ~ x
    out <- list()
    record <- function(id, area, target, actual, predicate) {
        warnings <- messages <- character()
        printed <- capture.output(value <- tryCatch(withCallingHandlers(actual,
            warning=function(c) {warnings <<- c(warnings,conditionMessage(c)); invokeRestart("muffleWarning")},
            message=function(c) {messages <<- c(messages,conditionMessage(c)); invokeRestart("muffleMessage")}),
            error=identity))
        failed <- inherits(value,"error")
        observed <- if (failed) trimws(conditionMessage(value)) else
            paste(paste(class(value),collapse="/"), paste(dim(value), collapse="x"),
                  if(is.atomic(value) && !is.language(value)) paste(utils::head(as.character(unclass(value)),8), collapse=",") else "")
        test <- tryCatch(predicate(value,warnings,messages,printed), error=function(e) FALSE)
        out[[length(out)+1L]] <<- data.frame(id,area,target,
            status=if(isTRUE(test)) "met" else if(failed) "error" else "mismatch",
            observed=gsub("[\t\r\n]+"," ",substr(observed,1,260)),
            warnings=paste(warnings,collapse=" | "),messages=paste(messages,collapse=" | "))
    }
    eq <- function(expected) function(v,...) identical(v,expected)
    near <- function(a,b) isTRUE(all.equal(unname(a),unname(b),tolerance=1e-12,check.attributes=FALSE))
    nums <- function(expected) function(v,...) is.matrix(v) && near(as.numeric(v),expected)
    reject <- function(v,...) inherits(v,"error")
    keys <- function(v) attr(v,"split",exact=TRUE)
    separateOrReject <- function(v,...) inherits(v,"error") || (is.matrix(v) && nums(c(3,4.25))(v))
    # Compare common input forms to independently evaluated expressions.
    for (obj in c("d","L","E","M","pair")) {
        data <- switch(obj,d=d,L=L,E=E,M=M,pair=as.pairlist(L))
        for (code in c("x","sum(x)","c(m=mean(x),s=sd(x))","list(vals=x, y=y)",
                       "{tmp <- x+1; sum(tmp)}","get(nm)","local({k<-2; x*k})")) {
            expr <- parse(text=code)[[1L]]
            refdata <- if(obj=="M") as.data.frame(data) else data
            expected <- eval(expr,refdata,environment())
            record(paste(obj,code), "unsplit", "same value and attributes as evaluation in data",
                   eval(call("using", quote(data), expr)), eq(expected))
        }
    }
    record("list-local", "unsplit","resolve helper from immediate caller",
           local({helper<-function(v) sum(v)+5; using(L,helper(x))}),eq(sum(d$x)+5))
    record("frame-local", "unsplit","resolve helper from immediate caller",
           local({helper<-function(v) sum(v)+5; using(d,helper(x))}),eq(sum(d$x)+5))
    record("matrix-local", "unsplit","resolve helper from immediate caller",
           local({helper<-function(v) sum(v)+5; using(M,helper(x))}),eq(sum(d$x)+5))
    record("list-assignment", "unsplit","assignment temporary",
           {using(L, x <- 0); L$x},eq(d$x))
    record("env-assignment", "unsplit","assignment mutates environment like with",
           {using(E, xx <- 4); E$xx},eq(4))
    record("empty-frame", "unsplit","evaluate length on zero rows as with",
           using(d[FALSE,],length(x)),eq(0L))
    for(obj in c("L","E","M","ragged")) {
        dat <- switch(obj,L=L,E=E,M=as.matrix(data.frame(x=d$x,g=as.integer(d$g))),
                      ragged=list(x=d$x,g=c("A","B")))
        record(paste0(obj,"-split"), "split", "honor split or reject explicitly",
               using(dat,mean(x),split.by=g),separateOrReject)
    }
    ref <- c(3,4.25)
    record("bare", "split","two ordered means",using(d,mean(x),split.by=g),nums(ref))
    record("quoted","split","honor character column name or reject",
           using(d,mean(x),split.by="g"),separateOrReject)
    record("transform","split","evaluate one derived group or reject",
           using(d,mean(x),split.by=g %in% c("A","B")),function(v,...) inherits(v,"error") ||
               (is.matrix(v) && length(v)==1 && near(as.numeric(v),mean(d$x))))
    record("external-split","split","local grouping vector visible",
           local({grp<-d$g;using(d,mean(x),split.by=grp)}),nums(ref))
    record("backticks","split","accept nonsyntactic grouping name",
           {dd<-d;names(dd)[3]<-"study arm";using(dd,mean(x),split.by=`study arm`)},nums(ref))
    record("reserved","split","honor column literally named split.by",
           {dd<-d;dd$split.by<-dd$g;using(dd,mean(x),split.by=split.by)},nums(ref))
    record("missing-name","split","reject unknown grouping name",using(d,mean(x),split.by=ghost),reject)
    record("all-missing","split","empty result or clear group diagnostic",
           {dd<-d;dd$g<-rep(NA_character_,8);using(dd,mean(x),split.by=g)},
           function(v,...) if(inherits(v,"error")) grepl("group|split|missing",conditionMessage(v),ignore.case=TRUE) else length(v)==0)
    record("right-groups","split","two-column grouping agrees with independent reference",
           using(d,sum(x),split.by=c(g,z)),nums(c(7,5,11,6)))
    record("and-groups","split","documented & grouping",using(d,sum(x),split.by=g & z),nums(c(7,5,11,6)))
    record("key-collision","split","recover each component of comma-containing group keys",
           {dd<-data.frame(x=1:2,a=c("p,q","p"),b=c("r","q,r"));using(dd,sum(x),split.by=c(a,b))},
           function(v,...) is.matrix(keys(v)) || !anyDuplicated(rownames(v)))
    record("dynamic-shadow","scope","use each group's x even with a conflicting caller x",
           using(d,mean(get("x")),split.by=g),nums(ref))
    record("dynamic-name","scope","look up run-time column",using(d,mean(get(nm)),split.by=g),nums(ref))
    record("local-helper-group","scope","local function sees group column",
           local({h<-function(v) sum(v)+1; using(d,h(x),split.by=g)}),nums(c(13,18)))
    record("expression-symbol","scope","a column named expr is usable",
           local({expr<-777;dd<-d;dd$expr<-dd$x;using(dd,expr)}),eq(d$x))
    record("namespace-call","scope","a namespaced summary works",
           using(d,base::mean(x),split.by=g),nums(ref))
    record("conditional-name","scope","programmatic selection from mask",
           using(d, mean(get(paste0("","x"))),split.by=g), nums(ref))
    # Models are assessed as usable objects, not simply by successful creation.
    truth <- lapply(groups,function(z) lm(y~x,data=z))
    fits <- using(d,lm(y~x),split.by=g)
    record("lm", "models","ungrouped coefficients match lm(data=)",
           coef(using(d,lm(y~x))),eq(coef(lm(y~x,data=d))))
    record("lm-groups","models","one intact fit per key with correct coefficients",
           fits,function(v,...) identical(keys(v),names(groups)) &&
               all(mapply(function(a,b) inherits(a,"lm") && near(coef(a),coef(b)),v,truth)))
    record("lm-predict","models","prediction on new data agrees",
           lapply(fits,predict,newdata=data.frame(x=9)),function(v,...) near(unlist(v),unlist(lapply(truth,predict,newdata=data.frame(x=9)))))
    record("lm-update","models","refit outside original group evaluation",
           lapply(fits,function(m) coef(update(m,.~1))),function(v,...) near(unlist(v),sapply(groups,function(g) mean(g$y))))
    record("with-update-control","controls","with() refit behaves like using() refit",
           tryCatch(update(with(d,lm(y~x)),.~1),error=identity),
           function(v,...) identical(inherits(v,"error"), inherits(tryCatch(update(using(d,lm(y~x)),.~1),error=identity),"error")))
    record("lm-dot","models","lm(y~.) uses group columns",
           using(d,lm(y~.),split.by=g),function(v,...) is.list(v) && all(vapply(v,inherits,logical(1),"lm")))
    record("with-dot-control","controls","base with() has same dot limitation",
           with(d,lm(y~.)),reject)
    record("lm-stored-formula","models","stored formula sees group-local variables",
           using(d,lm(formula0),split.by=g),
           function(v,...) is.list(v) && all(mapply(function(a,b) near(coef(a),coef(b)),v,truth)))
    record("lm-explicit-full-data","models","describe explicit external data binding",
           using(d,lm(y~x,data=d),split.by=g),function(v,...) all(vapply(v,nobs,integer(1))==nrow(d)))
    record("glm","models","glm objects and coefficients match reference",
           using(d,glm(y~x,family=poisson()),split.by=g),
           function(v,...) all(mapply(function(a,b) inherits(a,"glm") && near(coef(a),coef(glm(y~x,data=b,family=poisson()))),v,groups)))
    record("t-test","models","preserve htest result and estimate",
           using(d,t.test(x),split.by=g),function(v,...) all(mapply(function(a,b) inherits(a,"htest") && near(a$estimate,mean(b$x)),v,groups)))
    record("lm-subset","models","inline weights and subset track the group",
           using(d,lm(y~x,weights=x,subset=x>1),split.by=g),
           function(v,...) all(mapply(function(a,b) near(coef(a),coef(lm(y~x,weights=x,subset=x>1,data=b))),v,groups)))
    # Compare group output to evaluating the same expressions independently.
    # A values matrix is acceptable for plain same-schema vectors; classed values
    # and arrays must be recoverable with their full attributes.
    cases <- c(precise="mean(x)/7",tiny="1e-7*sum(x)",logical="all(x>0)",characters="as.character(max(x))",
        factor="factor(z)",date='as.Date("2020-01-01")+x',posix='as.POSIXct("2020-01-01",tz="UTC")+x',
        elapsed='as.difftime(x,units="hours")', complex='as.complex(x)+1i', raw='as.raw(x)',
        named='c(lo=min(x),hi=max(x))',matrix='cbind(x,y)',table='table(z)', data.frame='data.frame(x,y)',
        list='list(avg=mean(x),n=length(y))', model='lm(y~x)', nested='list(a=list(x),b=y)',
        null='NULL', empty='numeric(0)', mixednull='if(g[1]=="A") NULL else list(x)',
        ragged='x[x>2]', namedOrder='if(as.character(g[1])=="A") c(lo=1,hi=2) else c(hi=3,lo=4)',
        namedTable='table(as.character(x))', functionValue='function() x', expressionValue='quote(x+y)')
    for(id in names(cases)) {
        expr <- parse(text=cases[[id]])[[1L]]
        expected <- lapply(groups,function(z) eval(expr,z,environment()))
        pred <- function(v,...) {
            if(inherits(v,"error")) return(FALSE)
            if(is.list(v) && !is.matrix(v) && length(v)==length(expected)) {
                return(all(mapply(function(a,b) {
                    if(is.function(b)) return(is.function(a) && identical(a(),b()))
                    if(inherits(b,"lm")) return(inherits(a,"lm") && near(coef(a),coef(b)))
                    identical(a,b)
                },v,expected)))
            }
            if(!is.matrix(v) || nrow(v)!=length(expected)) return(FALSE)
            mat <- unclass(v)
            for(i in seq_along(expected)) {
                b<-expected[[i]]
                if(is.object(b) || !is.atomic(b) || is.null(b) || !is.null(dim(b)) || length(b)!=ncol(mat)) return(FALSE)
                a<-mat[i,,drop=TRUE]
                if(!is.null(names(b))) {
                    if(!identical(sort(names(a)),sort(names(b)))) return(FALSE)
                    a<-a[names(b)]
                }
                if(is.numeric(b) && !is.object(b)) {
                    if(!near(a,b)) return(FALSE)
                } else if(!identical(unname(a),unname(b))) return(FALSE)
            }
            TRUE
        }
        record(id,"returns","recover each group's reference value",eval(call("using",quote(d),expr,split.by=quote(g))),pred)
    }
    for (grouped in c(FALSE,TRUE)) {
        callit <- function(e) {
            cl<-call("using",quote(d),e)
            if(grouped) cl$split.by<-quote(g)
            eval(cl)
        }
        n <- if(grouped) 2L else 1L
        record(paste("warnings",grouped),"conditions","warnings reach caller once per evaluation",
               callit(quote({warning("precision"); sum(x)})),function(v,w,...) length(w)==n)
        record(paste("messages",grouped),"conditions","messages reach caller once per evaluation",
               callit(quote({message("fitting"); sum(x)})),function(v,w,m,...) length(m)==n)
        record(paste("error-class",grouped),"conditions","preserve custom condition class and message",
               callit(quote(stop(structure(list(message="bad object",call=NULL),class=c("bad_fit","error","condition"))))),
               function(v,...) inherits(v,"bad_fit") && identical(conditionMessage(v),"bad object"))
        record(paste("error-key",grouped),"conditions","group error identifies the failing group when split",
               callit(quote(if(any(x==8)) stop("failed") else mean(x))),
               function(v,...) inherits(v,"error") && (!grouped || grepl("B",conditionMessage(v),fixed=TRUE)))
        record(paste("invisible",grouped),"conditions","honor invisible()",
               withVisible(callit(quote(invisible(sum(x)))))$visible,eq(FALSE))
    }
    record("list-subset-keys","methods","model list retains matching keys after subsetting",
           fits[2],function(v,...) identical(keys(v),"B"))
    record("list-subset-print","methods","sublist prints correct group",
           capture.output(print(fits[2])),function(v,...) any(trimws(v)=="B"))
    record("matrix-drop","methods","explicit drop gives ordinary scalar",
           using(d,mean(x),split.by=g)[1,1,drop=TRUE],function(v,...) !is.matrix(v) && near(v,3))
    record("matrix-key-index","methods","character indexing preserves selected group label",
           using(d,mean(x),split.by=g)["B",,drop=FALSE],function(v,...) identical(rownames(v),"B") && near(as.numeric(v),4.25))
    record("side-effect-count","conditions","evaluate once per observed group",
           {calls<-0L; count<-function(x){calls<<-calls+1L;sum(x)};using(d,count(x),split.by=g);calls},eq(2L))
    record("print-block","methods","printed block keeps group headings and runs only once per group",
           using(d,{print(sum(x));invisible(1)},split.by=g),function(v,w,m,p) sum(trimws(p) %in% c("A","B"))==2L &&
               sum(grepl("\\[1\\] (12|17)$",p))==2L)
    record("tibble","packages","ordinary grouped expression",using(tibble::as_tibble(d),mean(x),split.by=g),nums(ref))
    record("data.table","packages","ordinary grouped expression",using(data.table::as.data.table(d),mean(x),split.by=g),nums(ref))
    record("MASS-rlm","packages","independent robust regression fits",
           using(d,MASS::rlm(y~x),split.by=g),function(v,...) all(mapply(function(a,b) inherits(a,"rlm") && near(coef(a),coef(MASS::rlm(y~x,data=b))),v,groups)))
    record("survival","packages","independent Kaplan-Meier fits",
           using(d,survival::survfit(survival::Surv(x,y>4)~1),split.by=g),function(v,...) all(mapply(function(a,b)
               inherits(a,"survfit") && identical(a$surv,survival::survfit(survival::Surv(x,y>4)~1,data=b)$surv),v,groups)))
    dd<-d;dd$grp<-declared::declared(c(1,1,9,1,2,2,2,2),labels=c(First=1,Second=2,Missing=9),na_values=9)
    record("declared-group","packages","labels and declared missing",
           using(dd,sum(x),split.by=grp),function(v,...) nums(c(8,17))(v)&&identical(rownames(v),c("First","Second")))
    dd<-d;dd$grp<-haven::labelled_spss(c(1,1,9,1,2,2,2,2),labels=c(First=1,Second=2),na_values=9)
    record("haven-group","packages","SPSS labels and user-missing codes",
           using(dd,sum(x),split.by=grp),function(v,...) nums(c(8,17))(v)&&identical(rownames(v),c("First","Second")))
    record("stored-formula-planted","models","group fits use group observations",
           local({f<-as.formula("y~x",env=list2env(list(y=d$y,x=d$x)));using(d,lm(f),split.by=g)}),
           function(v,...) all(mapply(function(a,b) nobs(a)==nobs(b)&&near(coef(a),coef(b)),v,truth)))
    record("stored-formula-control","controls","base lm with explicit group data overrides formula environment",
           local({f<-as.formula("y~x",env=list2env(list(y=d$y,x=d$x)));lapply(groups,function(z) lm(f,data=z))}),
           function(v,...) all(mapply(function(a,b) nobs(a)==nobs(b)&&near(coef(a),coef(b)),v,truth)))
    record("glm-warning","conditions","a real glm convergence warning reaches the caller",
           using(d,glm(y~x,family=poisson(),control=glm.control(maxit=1))),
           function(v,w,...) inherits(v,"glm") && !v$converged && length(w)>0)
    record("glm-warning-control","controls","direct glm signals nonconvergence on the same data",
           glm(y~x,data=d,family=poisson(),control=glm.control(maxit=1)),
           function(v,w,...) inherits(v,"glm") && !v$converged && length(w)>0)
    record("synthetic-interrupt","conditions","interrupt-class condition reaches enclosing interrupt handler",
           tryCatch(using(d,stop(structure(list(message="cancel",call=NULL),class=c("interrupt","condition")))),
                    interrupt=function(e) "propagated"), eq("propagated"))
    record("result-choice","returns","a mixture of numeric and character results remains recoverable",
           using(d,if(as.character(g[1])=="A") 1L else "1",split.by=g),
           function(v,...) is.list(v)&&identical(v[[1]],1L)&&identical(v[[2]],"1"))
    record("table-alignment","returns","category counts agree by name for both groups",
           {dd<-data.frame(g=rep(c("A","B"),each=3),q=c("a","a","b","b","c","c"));
            using(dd,table(q),split.by=g)}, function(v,...) {
                if(is.list(v)) return(identical(names(v[[2]]),c("b","c")) && identical(as.integer(v[[2]]),1:2))
                is.matrix(v) && all(c("a","b","c") %in% colnames(v)) && near(unclass(v)["B",c("a","b","c")],c(0,1,2))
            })
    record("list-nested-input","unsplit","preserve nested nonrectangular list semantics",
           using(list(a=list(x=1:3),b="hello"),list(sum(a$x),b)),eq(list(6L,"hello")))
    record("duplicated-names","unsplit","match base lookup for duplicate names",
           {v<-structure(list(5L,6L),names=c("x","x"));using(v,x)},eq(with(structure(list(5L,6L),names=c("x","x")),x)))
    record("list-column","returns","preserve a group-local list column nested in the result",
           {dd<-d;dd$v<-as.list(dd$x);using(dd,list(v),split.by=g)},
           function(v,...) identical(v[[2]][[1]],as.list(groups$B$x)))
    record("tidy-mask","packages","a tidy-evaluation column lookup reads the current group or diagnoses missing pronoun",
           using(d, mean(.data$x), split.by=g), function(v,...) inherits(v,"error") || nums(ref)(v))
    record("forward-wrapper","scope","a wrapper forwards its split name and expression as intended",
           local({wrap<-function(dat, expression, key) using(dat,expression,split.by=key)
                  wrap(d,mean(x),g)}),nums(ref))
    record("unequal-input","split","ungrouped heterogeneous list keeps ordinary list semantics",
           using(list(a=1:2,b=10:14),length(b)-length(a)),eq(3L))
    record("statistics","packages","grouped custom function matches independent base evaluations",
           using(d,list(statistics::spooled(sd(x),sd(y),length(x),length(y))),split.by=g),
           function(v,...) all(mapply(function(a,b) near(a[[1]],statistics::spooled(sd(b$x),sd(b$y),nrow(b),nrow(b))),v,groups)))
    cases <- do.call(rbind, out)
    for (i in seq_len(nrow(cases))) {
        expect_true(cases$status[i] == "met",
            info = paste(cases$id[i], cases$target[i], cases$observed[i], sep = " | "))
    }
    # Generated ordinary groups, missing keys, sparse levels, shuffled order and repeated values.
    # Keep the strongest oracle simple: observed factor order and exact integer sums.
    results<-integer(3)
    for(i in seq_len(250)) {
        n<-sample(3:80,1); z<-data.frame(x=sample(-8:12,n,TRUE),g=factor(sample(c(LETTERS[1:6],NA),n,TRUE),levels=LETTERS[6:1]))
        zz<-split(z$x,z$g,drop=TRUE)
        calc<-function(code) tryCatch(eval(call("using",quote(z),code,split.by=quote(g))),error=function(e) NULL)
        for(j in 1:3) {
            e<-list(quote(sum(x)),quote(mean(x)/7),quote(sum(get("x"))))[[j]]
            v<-calc(e); target<-vapply(zz,function(xs) if(j==2) mean(xs)/7 else sum(xs),numeric(1))
            ok <- is.matrix(v) && identical(rownames(v), names(zz)) && near(as.numeric(v), target)
            expect_true(ok, info = sprintf("generated data set %d, expression %s", i, deparse(e)))
            results[j] <- results[j] + as.integer(ok)
        }
    }
    reports <- getOption("admisc.stress_results")
    if (!is.null(reports)) {
        generated <- data.frame(operation = c("sum", "mean/7", "sum(get(x))"),
                                met = results, total = 250L)
        write.table(cases, file.path(reports, "results.tsv"), sep = "\t",
                    row.names = FALSE, qmethod = "double")
        write.table(generated, file.path(reports, "random.tsv"), sep = "\t",
                    row.names = FALSE, quote = FALSE)
    }
})
