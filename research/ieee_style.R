if(!require(rmarkdown))
  install.packages("rmarkdown")
if(!require(RefManageR))
  remotes::install_github("ropensci/RefManageR")

dfolio <- utils::person("David", "Folio", role=c("aut"),
                        email="david.folio@insa-cvl.fr",
                        comment = c(ORCID = "0000-0001-9430-6091"))

supervised <- data.frame(person=c(person("Karim", "Belharet", role=c("aut", "dgs"), comment=c(ORCID="0000-0003-0348-5157")),
                                  person("Nabil", "Amari", role=c("aut", "dgs")),
                                  person("Jungsik", "Kim", role=c("aut", "dgs")),
                                  person("Christian", "Dahmen", role=c("aut", "dgs")),
                                  person("Tao", "Li", role=c("aut", "dgs")),
                                  person("Bumjin", "Jang", role=c("aut", "dgs")),
                                  person("Lyès", "Mellal", role=c("aut", "dgs"), comment=c(ORCID="0000-0002-6330-930X")),
                                  person("Bruno", "Sarkis", role=c("aut", "dgs")),
                                  person("Jiaen", "Wu", role=c("aut", "dgs")),
                                  person("Ruipeng", "Chen", role=c("aut", "dgs")),
                                  person("Karim", "Botros", role=c("aut", "dgs"))),
                         end  =c("2014", "2017", "2013", "2017", "2017", "2017",  "2022", "2019", "2023", "2023", "2030"))



refM <- environment()
ieee_style <- RefManageR:::MakeBibLaTeX(docstyle = "markdown", authortitle = FALSE)

.is_not_nonempty_text <- function(x) is.null(x) || anyNA(x) || all(grepl("^[[:space:]]*$", x))

addColon <- function(s){
  ## #62: sub can cause conversion of unicode char to latin1 on windows
  ## sub("([^.?!])$", "\\1.", string, useBytes = FALSE)
  if (!.is_not_nonempty_text(s) && grepl("([^.?!])$", s, useBytes = FALSE))
    paste0(s, ":")
  else
    s
}
sentencePar<-function(..., sep=', '){
  strings <- c(...)
  res <- NULL
  if (length(strings))
    res <- paste(strings, collapse = sep)
  res <- gsub('\\.$', '', res, useBytes = FALSE)
  paste0("(",res, ")")
}

fmtSetClass <- function(x, class, prefix="bib-"){
  if (length(x))
    paste0("[", x, "]\\{.",prefix, class,"\\}")
}
fmtNumPre <- function(doc){
  res <- doc$shorthand
  if (!length(res))
    res <- doc$.index
  res
}

fmtURI <- function(label, uri, prefix="", uri_prefix="", class="uri"){
  if (length(uri))
    fmtSetClass(paste0("[", prefix, label, "](",uri, ")\\{rel='noopener noreferrer'\\}"),class)
}

with(ieee_style,
     ieee_style$setIndex<-function (x) {
       ps <- character(length(x))
       for (i in seq_along(x)) {
         res <- x[[i]]$presort
         if (!length(res)){
           res <- x[[i]]$sortkey
           if (!length(res))
             res <- "mm"
         }
         ps[i] <- res
       }
       yr <- character(length(x))
       bib <- unclass(x)
       for (i in seq_along(bib)) {
         res <- bib[[i]]$sortyear
         if (!length(res)){
           res <- attr(bib[[i]], 'dateobj')
           if (is.null(res)){
             res <- 9999
           }else if (inherits(res, 'Interval')){
             res <- lubridate::year(int_start(res))
           }else{
             res <- lubridate::year(res)
           }
         }
         yr[i] <- res
       }
       aut <- character(length(x))
       for (i in seq_along(x)) {
         authors <- x[[i]]$sortname
         if (!length(authors)){
           if (length(x[[i]]$author)>1){
             me_idx<-match(dfolio$family,x[[i]]$author$family)
             x[[i]]$author[me_idx] <- dfolio

             aut_idx<-Reduce(`|`, lapply(supervised$person$family, grepl, x =x[[i]]$author$family)) &
               Reduce(`|`, lapply(supervised$person$given, grepl, x =x[[i]]$author$given))

             if (length(x[[i]]$author$family[aut_idx])){
               sup_idx2<-Reduce(`|`, lapply(x[[i]]$author$family[aut_idx], grepl, x =supervised$person$family))
               sup_idx3<- sup_idx2 & supervised$end > x[[i]]$date
               sup<-supervised
               sup$person[!sup_idx3]$role <- c("aut")
               sup<-sup$person[sup_idx2]
               # cat("[",i,"]idx1: ", aut_idx, "(",length(x[[i]]$author$family[aut_idx]),")",
               #     "; dx2: ", sup_idx2, "\n" )
               if (length(sup))
                x[[i]]$author[aut_idx]<-rev(sup)
             }
           }
           authors <- paste0(vapply(x[[i]]$author, shortNameLF, ""), collapse = '')
         }
         if (authors == ''){
           authors <- paste0(vapply(x[[i]]$editor, shortNameLF, ""), collapse = '')
           if (authors == '')
             authors <- paste0(vapply(x[[i]]$translator, shortNameLF, ""), collapse = '')
         }
         aut[i] <- authors

       }
       ttl <- character(length(x))
       for (i in seq_along(x)){
         res <- x[[i]]$sorttitle
         if (!length(res))
           res <- x[[i]]$title
         if (!length(res)){
           res <- x[[i]]$maintitle
           if (!length(res))
             res <- x[[i]]$booktitle
           if (!length(res))
             res <- ""
         }
         ttl[i] <- res
       }
       ydnt = order(ps, -as.numeric(yr), aut, ttl,decreasing = FALSE)
       x <-x[ydnt]

       ind <- which(!unlist(x$bibtype) %in% c('Set', 'XData'))
       index <- numeric(length(x))
       index[ind] <- seq_along(ind)
       x$.index <-index
       x
     })
refM$shortNameLF=ieee_style$shortNameLF
refM$shortName=ieee_style$shortName
with(ieee_style,
     ieee_style$shortName<- function(pers){
       if (pers$family==dfolio$family){
         fmtSetClass(refM$shortName(pers),"me")
       }else if( length(which("dgs" %in% pers$role))){
         fmtSetClass(refM$shortName(pers),"supervised")
       }else{
         fmtSetClass(refM$shortName(pers),"contrib")
       }
     })

#
with(ieee_style,
     ieee_style$fmtPrefix <- function(paper){
       res <- paste0("[", fmtNumPre(paper), "]")
       if (length(res)){
         key <- attr(paper, "key")
         ind <- paper$.index
         key <- gsub("[^_a-zA-Z0-9-]", "", key, useBytes = FALSE)
         res <-paste0("<a name=bib-", key, "></a>[", res, "]\\{#cite-", key, "\\}")
       }
       fmtSetClass(res,"prefix")
     })
refM$fmtBAuthor=ieee_style$fmtBAuthor
with(ieee_style,
     ieee_style$fmtBAuthor <- function(s) fmtSetClass(refM$fmtBAuthor(s),"authors"))

with(ieee_style,
     ieee_style$fmtBAuthorSimple <- function(doc, max.n){
       # if (doc$.duplicated)
       #   return(switch(docstyle, html = "---", markdown = "\\-\\-\\-",
       #                 "\u2014\u2013\u2014"))
       out <- NULL
       if (length(doc$author)){
         res <- doc$author
       }else if (length(doc$editor)){
         res <- doc$editor
       }else if (length(doc$translator)){
         res <- doc$translator
       }else{
         return()
       }
       nnames <- length(res)
       if (nnames){
         if (bibstyle != "authortitle"){
           out <- shortName(res[1L])
         }else{
           out <- shortNameLF(res[1L])
         }
         if (nnames == 2L){
           if (max.n < 2L){
             out <- paste0(out, ' et al.')
           }else{
             out <- paste(out, shortName(res[-1L]), sep = ' and ')
           }
         }else if (nnames > 2L){
           if (nnames > max.n){
             if (max.n <= 1L){
               out <- paste0(out, ' et al.')
             }else{
               out <- paste0(paste(out, paste0(vapply(res[2L:max.n],
                                                      shortName, ""),
                                               collapse = ", "), sep = ', '),
                             ', et al.')
             }
           }else{
             out <- paste(paste(out, paste0(vapply(res[-c(1L, length(res))],
                                                   shortName, ""),
                                            collapse = ", "), sep = ', '),
                          shortName(res[length(res)]), sep = ', and ')
           }
         }
       }

       if (!length(doc$author) && length(doc$editor)){
         if (!length(doc$editortype)){
           out <- paste0(out, ', ed.')
         }else{
           out <- paste0(out, switch(tolower(doc$editortype),
                                     'compiler' = ', comp.',
                                     'editor' = ', ed.',
                                     'founder' = ', found.',
                                     'continuator' = ', cont.',
                                     'redactor' = ', red.',
                                     'reviser' = ', rev.',
                                     'collaborator' = ', collab.',
                                     doc$editortype))

         }
       }else{
         out <- addPeriod(out)
       }
       out
     })

refM$fmtJTitle=ieee_style$fmtJTitle
with(ieee_style,
     ieee_style$fmtJTitle <- function(p, class="title", quote=TRUE) {
       if (length(p$title)){
         res <- (cleanupLatex(p$title))
         if (length(p$subtitle))
           res <- paste0(addColon(res), (cleanupLatex(p$subtitle)), collapse =' ')
         if (quote)
           res<-refM$fmtJTitle(res)
         else
           res<-addPeriod(res)
         if (!length(p$url) && file.exists(paste0("../publications/articles/",attr(p, "key"),".qmd")))
           p$url<-paste0("../publications/articles/",attr(p, "key"),".qmd")
         if (length(p$url))
           fmtURI(label=res, uri=p$url, class=class)
         else
           fmtSetClass(res,class)
       }
     })
with(ieee_style,
     ieee_style$fmtBTitle <- function(t,st, url=NULL, quote=FALSE, class="booktitle") {
       if (length(t)){
         res <- (cleanupLatex(t))
         if (length(st))
           res <- paste0(addColon(res), (cleanupLatex(st)), collapse =' ')
         if (quote)
           res<-refM$fmtJTitle(res)
         else
           res<-addPeriod(res)
         if (length(url))
           fmtURI(label=res, uri=url, class=class)
         else
           fmtSetClass(res,class)
       }
     })
with(ieee_style,
     ieee_style$fmtIBTitle <- function(p, b=FALSE, quote=FALSE, class="inbooktitle") {
       if (!length(p$url) && file.exists(paste0(getwd(),"/../publications/conferences/",attr(p, "key"),".qmd")))
         p$url<-paste0("../publications/conferences/",attr(p, "key"),".qmd")
       if (b){  # bookinbook entry
         ieee_style$fmtBTitle(p$title, p$subtitle, url=p$url, class=class)
       }else{
         ieee_style$fmtJTitle(p, class=class, quote=quote)
       }
     })
with(ieee_style,
     ieee_style$fmtJournal <- function(s){
       if (length(s$journaltitle)) {
         res <- (cleanupLatex(s$journaltitle))
         if (length(s$journalsubtitle))
           res <- paste((res), (cleanupLatex(s$journalsubtitle)))
         fmtSetClass(res,"journal")
       }
       else if (!is.null(s$journal)) {
         fmtSetClass(cleanupLatex(s$journal),"journal")
       }
     })
refM$fmtSeries=ieee_style$fmtSeries
with(ieee_style,
     ieee_style$fmtSeries <- function(s) fmtSetClass(refM$fmtSeries(s),"series"))
with(ieee_style,
     ieee_style$fmtVolume <- function(v, n) {
       if (length(v))
         v<-paste0("Vol.",v)
       if (length(n))
         n<-paste0("no.",n)
       sentence(fmtSetClass(v,"vol"),fmtSetClass(n,"num"), sep=' ')
     })
refM$fmtJournDate=ieee_style$fmtJournDate
with(ieee_style,
     ieee_style$fmtJournDate <- function(s, usevenue = FALSE){
       res <- DateFormatter(attr(s, 'dateobj'))
       if (usevenue){
         fmtSetClass(paste0(c(cleanupLatex(gsub('[.?!]$', '', s[['venue']],useBytes = FALSE)),
                              res), collapse = ', '), "date")
       }else{
         fmtSetClass(paste0(c(s[['issue']], res), collapse = ' '), "date")
       }
     })
refM$fmtDate=ieee_style$fmtDate
with(ieee_style,
     ieee_style$fmtDate <- function(d){
       fmtSetClass(refM$fmtDate(d),"date")
     })

refM$fmtPages=ieee_style$fmtPages
with(ieee_style,
     ieee_style$fmtPages <- function(p, pn)  fmtSetClass(refM$fmtPages(p, pn),"pages"))
with(ieee_style,
     ieee_style$fmtNote <- function(n, prefix="",suffix="")  {
       if (length(n))
         fmtSetClass(paste0(prefix, cleanupLatex(n), suffix),"note")
     })
with(ieee_style,
     ieee_style$fmtDOI <- function(doi){
       if (length(doi)){
         ##   need to gsub/escape % for  later call to tools::Rd2txt
         enc.doi <- gsub("%", "\\\\%", URLencode(doi, reserved = TRUE))
         #fmtSetClass(paste0("DOI: [", doi, "](https://doi.org/", enc.doi, ")\\{.bib-doi-url\\}."),"doi")
         fmtURI(label=doi, uri=paste0("https://doi.org/", enc.doi), prefix="\\{\\{< ai doi >\\}\\}", class="doi")
       }
     })
with(ieee_style,
     ieee_style$fmtHAL <- function(hal){
       if (length(hal)){
         fmtURI(label=hal, uri=paste0("https://hal.archives-ouvertes.fr/", hal), prefix="\\{\\{< ai hal >\\}\\}", class="hal")
       }
     })
with(ieee_style,
     ieee_style$fmtPublisher <- function(pub, loc, addr) {
       if (length(loc) || length(addr)){
         res <- if (length(loc))
           plainclean(loc)
         else plainclean(addr)
         if (length(pub))
           res <- paste(plainclean(pub), res,  sep = ': ')
         paste0(res, ', ')
       }else if (length(pub)){
         paste0(plainclean(pub), ', ')
       }
     })

# with(ieee_style,
#      ieee_style$fmtISSN <- function(s) fmtSetClass(refM$fmtISSN(s),"issn"))
with(ieee_style,
     ieee_style$fmtSeries <- function(s){
       if (length(s))
         paste0("Series in ", s)
     })
with(ieee_style,
     ieee_style$fmtChapter <- function(s){
       if (length(s))
         paste0("Chap. ", s)
     })
refM$fmtType=ieee_style$fmtType
with(ieee_style,
     ieee_style$fmtType <- function(t){
       fmtSetClass(refM$fmtType(t),"type")
     })
with(ieee_style,
     ieee_style$fmtHowPublished <- function(s){
       if (length(s))
         fmtSetClass( cleanupLatex(s),"howpublished")
     })
with(ieee_style,
     ieee_style$fmtEventTitle <- function(s){
       if (length(s))
         fmtSetClass( cleanupLatex(s),"eventtitle")
     })
with(ieee_style,
     ieee_style$fmtLanguage <- function(lang){
       if (length(lang)){
         lang<-tolower(lang)
         if (lang != 'english' && lang != 'en' && lang != 'american' && lang != 'en-us')
           paste0("^[",lang,"]\\{.badge .rounded-pill .bg-primary\\}^")
       }
     })
with(ieee_style,
     ieee_style$formatArticle <- function(paper){
       collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtJTitle(paper),
                  fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
                  fmtTranslator(paper), fmtCommentator(paper$commentator),
                  fmtAnnotator(paper$annotator), fmtVersion(paper$version),
                  sentenceP(paste0(c(fmtJournal(paper), fmtSeries(paper$series),
                                     fmtVolume(paper$volume, paper$number),
                                     fmtPages(paper$pages, paper$pagination),
                                     fmtEditor(paper, suffix = NULL, prefix = '. ')),
                                   collapse =' ', sep = ', '),
                            fmtJournDate(paper),
                            fmtBTitle(paper$issuetitle, paper$issuesubtitle, paper$url),
                            fmtEditor(paper, suffix = NULL, prefix = '. '),

                            sep = ''),
                  fmtISSN(paper$issn), fmtDOI(paper$doi), fmtEprint(paper),
                  fmtURL(paper), fmtHAL(paper$hal_id), fmtAddendum(paper$addendum),
                  ieee_style$fmtNote(paper$note),
                  fmtPubstate(paper$pubstate)
       ))
     })
with(ieee_style,
     ieee_style$formatBook <- function(paper, collection = FALSE){
       class="booktitle"
       if (collection && length(paper$author)){
         paper$author <- NULL
         class="collectiontitle"
       }
       collapse(c(fmtPrefix(paper), fmtBAuthor(paper),
                  fmtBTitle(paper$maintitle, paper$mainsubtitle, quote=TRUE, class=class),
                  fmtAddOn(paper$maintitleaddon),
                  paste0(c(fmtBVolume(paper$volume, paper$part),
                           fmtBTitle(paper$title, paper$subtitle)),
                         collapse = ': '), fmtAddOn(paper$titleaddon),
                  fmtLanguage(paper$language),
                  fmtEditor(paper, !length(paper$author)),
                  fmtTranslator(paper), fmtCommentator(paper$commentator),
                  fmtAnnotator(paper$annotator),
                  fmtIntroduction(paper$introduction),
                  fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
                  fmtEdition(paper$edition), fmtVolumes(paper$volumes),
                  sentence(cleanupLatex(paper$series), paper$number, sep = ' '),
                  sentence(fmtPublisher(paper$publisher, paper$location,
                                        paper$address),
                           fmtDate(attr(paper, 'dateobj')),
                           fmtChapter(paper$chapter),
                           fmtPages(paper$pages, paper$bookpagination),
                           fmtTotalPages(paper$pagetotal, paper$bookpagination),
                           sep = ''),
                  fmtISBN(paper$isbn), fmtDOI(paper$doi), fmtEprint(paper),
                  fmtURL(paper), fmtHAL(paper$hal_id),
                  fmtNote(paper$note),fmtAddendum(paper$addendum), fmtPubstate(paper$pubstate)
       ))
     })
with(ieee_style,
     ieee_style$formatInBook <- function(paper, bookinbook = FALSE){
       collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtIBTitle(paper, bookinbook),
                  fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
                  paste0(c('In ', fmtIBAuthor(paper$bookauthor),
                           fmtBTitle(paper$maintitle, paper$mainsubtitle, paper$url))),
                  fmtAddOn(paper$maintitleaddon),
                  paste0(c(fmtBVolume(paper$volume, paper$part),
                           fmtBTitle(paper$booktitle, paper$booksubtitl, paper$urle)),
                         collapse = ': '), fmtAddOn(paper$booktitleaddon),
                  fmtEditor(paper, !length(paper$author)),
                  fmtTranslator(paper), fmtCommentator(paper$commentator),
                  fmtAnnotator(paper$annotator),
                  fmtIntroduction(paper$introduction),
                  fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
                  fmtEdition(paper$edition),
                  sentence(fmtVolume(paper$volume, paper$number), fmtSeries(cleanupLatex(paper$series))),
                  sentenceP(fmtPublisher(paper$publisher, paper$location,
                                         paper$address), fmtDate(attr(paper, 'dateobj')),
                            fmtChapter(paper$chapter),
                            pgs = fmtPages(paper$pages, paper$bookpagination),
                            tp = fmtTotalPages(paper$pagetotal,
                                               paper$bookpagination), sep = ''),
                  fmtISBN(paper$isbn), fmtDOI(paper$doi), fmtEprint(paper),
                  fmtURL(paper), fmtHAL(paper$hal_id), fmtNote(paper$note),
                  fmtAddendum(paper$addendum), fmtPubstate(paper$pubstate)
       ))
     })
with(ieee_style,
     ieee_style$formatInCollection <- function(paper){
       collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtIBTitle(paper),
                  fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
                  sentence(fmtChapter(paper$chapter),fmtVolume(paper$volume, paper$number)),
                  paste0(c('In ', fmtBTitle(paper$maintitle, paper$mainsubtitle, quote=TRUE, class="journal"))),
                  fmtAddOn(paper$maintitleaddon),
                  paste0(c(fmtBVolume(paper$volume, paper$part),
                           fmtBTitle(paper$booktitle, paper$booksubtitle)),
                         collapse = ': '), fmtAddOn(paper$booktitleaddon),
                  fmtEditor(paper, !length(paper$author)),
                  fmtTranslator(paper), fmtCommentator(paper$commentator),
                  fmtAnnotator(paper$annotator),
                  fmtIntroduction(paper$introduction),
                  fmtForeword(paper$foreword), fmtAfterword(paper$afterword),
                  sentenceP(fmtEdition(paper$edition), fmtSeries(cleanupLatex(paper$series)),
                            fmtPublisher(paper$publisher, paper$location, paper$address),
                            fmtDate(attr(paper, 'dateobj')),
                            pgs = fmtPages(paper$pages,paper$bookpagination),
                            tp = fmtTotalPages(paper$pagetotal,paper$bookpagination),
                            sep = ''), fmtISBN(paper$isbn),
                  fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtHAL(paper$hal_id),
                  fmtNote(paper$note), fmtAddendum(paper$addendum), fmtPubstate(paper$pubstate)
       ))
     })
with(ieee_style,
     ieee_style$formatInProceedings <- function(paper){
       collapse(c(fmtPrefix(paper), fmtBAuthor(paper),fmtIBTitle(paper, b=FALSE, class='title'),
                  fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
                  paste0(c('In ', fmtBTitle(paper$maintitle, paper$mainsubtitle))),
                  fmtAddOn(paper$maintitleaddon),
                  paste0(c(fmtBVolume(paper$volume, paper$part),
                           fmtBTitle(paper$booktitle, paper$booksubtitle, class='proceedingtitle')),
                         collapse = ': '), fmtAddOn(paper$booktitleaddon),
                  fmtEventTitle(paper$eventtitle),
                  sentence(paper$eventtitleaddon, fmtEventDate(paper$eventdate, paper$venue), sep = ' '),
                  fmtEditor(paper, !length(paper$author)), fmtTranslator(paper),
                  fmtCommentator(paper$commentator),
                  fmtAnnotator(paper$annotator),
                  fmtIntroduction(paper$introduction),
                  fmtForeword(paper$foreword),
                  fmtAfterword(paper$afterword), fmtVolumes(paper$volumes),
                  sentenceP(cleanupLatex(paper$series), fmtVolume(paper$volume, paper$number),
                            fmtChapter(paper$chapter),
                            pgs = fmtPages(paper$pages, paper$bookpagination),
                            tp = fmtTotalPages(paper$pagetotal,
                                               paper$bookpagination), sep = ''),
                  fmtPublisher(paper$publisher, paper$location, paper$address),
                  fmtOrganization(paper$organization),
                  fmtDate(attr(paper, 'dateobj')),
                  fmtISBN(paper$isbn), fmtDOI(paper$doi), fmtEprint(paper),
                  fmtURL(paper), fmtHAL(paper$hal_id), fmtAddendum(paper$addendum),
                  fmtNote(paper$note), fmtPubstate(paper$pubstate)
       ))
     })
with(ieee_style,
     ieee_style$formatUnpublished <- function(paper){
       collapse(c(fmtPrefix(paper), fmtBAuthor(paper),
                  fmtIBTitle(paper, b=FALSE),
                  fmtAddOn(paper$titleaddon), fmtLanguage(paper$language),
                  sentence(fmtEventTitle(paper$eventtitle),
                           sentencePar(fmtHowPublished(paper$howpublished),
                                       paper$eventtitleaddon,
                                       fmtEventDate(paper$eventdate, paper$venue),
                                       sep = ' '),
                           sep = ' '),
                  sentence(fmtPublisher(NULL, paper$location, paper$address),
                           fmtDate(attr(paper, 'dateobj')), sep = ''),
                  fmtDOI(paper$doi), fmtURL(paper), fmtNote(paper$note), fmtAddendum(paper$addendum),
                  fmtPubstate(paper$pubstate)
       ))
     })
with(ieee_style,
     ieee_style$formatThesis <- function(paper, type = NULL){
       if (length(type))
         paper$type <- type
       if (!is.null(paper$school) && is.null(paper$institution))
         paper$institution <- paper$school
       if(length(paper$institution) && !is.null(paper$type))
         paper$type<- paste0(paper$type, " of")
       collapse(c(fmtPrefix(paper), fmtBAuthor(paper),fmtIBTitle(paper, b=FALSE),
                  fmtAddOn(paper$titleaddon),
                  fmtLanguage(paper$language), (fmtType(paper$type)),
                  fmtNote(paper$note),
                  sentenceP(fmtPublisher(paper$institution, paper$location,
                                         paper$address),
                            fmtDate(attr(paper, 'dateobj')),
                            fmtChapter(paper$chapter),
                            pgs = fmtPages(paper$pages, paper$bookpagination),
                            tp = fmtTotalPages(paper$pagetotal,
                                               paper$bookpagination), sep = ''),
                  fmtDOI(paper$doi), fmtEprint(paper), fmtURL(paper), fmtHAL(paper$hal_id),
                  fmtAddendum(paper$addendum), fmtPubstate(paper$pubstate)
       ))
     })

tools::bibstyle("ieee", ieee_style)
RefManageR::BibOptions(check.entries = FALSE, style = "markdown", sorting = "ydnt", max.names=99, bib.style = "ieee")
bib <- RefManageR::ReadBib("/home/dfolio/Documents/WWW/dfolio-website/research/dfolio-md.bib", .Encoding = "UTF-8", check = 'warn')
