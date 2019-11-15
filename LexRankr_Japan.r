LexRankr_Japan <- function (text, docId = "create", threshold = 0.2, n = 3, returnTies = TRUE, 
    usePageRank = TRUE, damping = 0.85, continuous = FALSE, sentencesAsDocs = FALSE, 
    removePunc = TRUE, removeNum = TRUE, toLower = TRUE, stemWords = TRUE, 
    rmStopWords = TRUE, Verbose = TRUE) 
{
    if (!is.logical(Verbose)) 
        stop("Verbose must be logical")
    if (length(Verbose) != 1) 
        stop("Verbose must be length 1")
    if (Verbose) 
        cat("Parsing text into sentences and tokens...")
    sentTokList <- my_sentenceTokenParse(text = text, docId = docId, 
        removePunc = removePunc, removeNum = removeNum, toLower = toLower, 
        stemWords = stemWords, rmStopWords = rmStopWords)
    if (Verbose) 
        cat("DONE\n")
    sentDf <- sentTokList$sentences
    tokenDf <- sentTokList$tokens
    if (Verbose) 
        cat("Calculating pairwise sentence similarities...")
    similDf <- sentenceSimil(sentenceId = tokenDf$sentenceId, 
        token = tokenDf$token, docId = tokenDf$docId, sentencesAsDocs = sentencesAsDocs)
    if (Verbose) 
        cat("DONE\n")
    if (Verbose) 
        cat("Applying LexRank...")
    topNSents <- lexRankFromSimil(s1 = similDf$sent1, s2 = similDf$sent2, 
        simil = similDf$similVal, threshold = threshold, n = n, 
        returnTies = returnTies, usePageRank = usePageRank, damping = damping, 
        continuous = continuous)
    if (Verbose) 
        cat("DONE\nFormatting Output...")
    returnDf <- merge(sentDf, topNSents, by = "sentenceId")
    returnDf <- returnDf[order(-returnDf$value), c("docId", "sentenceId", 
        "sentence", "value")]
    rownames(returnDf) = NULL
    if (Verbose) 
        cat("DONE\n")
    return(returnDf)
}

my_sentenceTokenParse <- function (text, docId = "create", removePunc = TRUE, removeNum = TRUE, 
    toLower = TRUE, stemWords = TRUE, rmStopWords = TRUE) 
{
    sentenceDf <- my_sentenceParse(text, docId = docId) 
    tokenDfList <- lapply(seq_along(sentenceDf$sentence), function(i) {
        sentVec <- sentenceDf$sentence[i]
        tokenList <- my_tokenize(text = sentVec, removePunc = removePunc, 
            removeNum = removeNum, toLower = toLower, stemWords = stemWords, 
            rmStopWords = rmStopWords)
        subTokenDfList <- lapply(seq_along(tokenList), function(j) {
            data.frame(docId = sentenceDf$docId[i], sentenceId = sentenceDf$sentenceId[i], 
                token = tokenList[[j]], stringsAsFactors = FALSE)
        })
        do.call("rbind", subTokenDfList)
    })
    tokenDf <- do.call("rbind", tokenDfList)
    tokenDf <- tokenDf[!is.na(tokenDf$token), ]
    class(tokenDf) <- "data.frame"
    list(sentences = sentenceDf, tokens = tokenDf)
}

my_sentenceParse <- function (text, docId = "create") 
{
    if (!is.character(text)) 
        stop("text must be character")
    if (length(text) < 1) 
        stop("text must be at least length 1")
    docId <- as.character(docId)
    if (length(docId) == 1 & docId[1] == "create") {
        createDocIds <- TRUE
    }
    else if (length(docId) == length(text)) {
        createDocIds <- FALSE
    }
    else if (length(docId) != length(text)) 
        stop("docId vector must be same length as text vector")
    sentences <- my_sentence_parser(text)
    sentenceDfList <- lapply(seq_along(sentences), function(i) {
        sentVec <- trimws(sentences[[i]])
        if (length(sentVec) == 0) 
            sentVec = ""
        if (createDocIds) {
            out = data.frame(docId = i, sentenceId = paste0(i, 
                "_", seq_along(sentVec)), sentence = sentVec, 
                stringsAsFactors = FALSE)
        }
        else if (!createDocIds) {
            out = data.frame(docId = docId[i], sentence = sentVec, 
                stringsAsFactors = FALSE)
        }
        out
    })
    sentenceDf <- do.call("rbind", sentenceDfList)
    sentenceDfList <- split(sentenceDf, sentenceDf$docId)
    sentenceDfList <- lapply(sentenceDfList, function(dfi) {
        dfi$sentenceId <- paste0(dfi$docId, "_", 1:nrow(dfi))
        dfi[, c("docId", "sentenceId", "sentence")]
    })
    sentenceDf <- do.call("rbind", sentenceDfList)
    class(sentenceDf) <- "data.frame"
    rownames(sentenceDf) <- NULL
    return(sentenceDf)
}

my_tokenize <- function (text, removePunc = TRUE, removeNum = TRUE, toLower = TRUE, 
    stemWords = TRUE, rmStopWords = TRUE) 
{
    if (!is.character(text)) 
        stop("text must be character")
    if (length(text) < 1) 
        stop("text must be at least length 1")
    if (!is.logical(removePunc)) 
        stop("removePunc must be logical")
    if (length(removePunc) != 1) 
        stop("removePunc must be length 1")
    if (!is.logical(removeNum)) 
        stop("removeNum must be logical")
    if (length(removeNum) != 1) 
        stop("removeNum must be length 1")
    if (!is.logical(toLower)) 
        stop("toLower must be logical")
    if (length(toLower) != 1) 
        stop("toLower must be length 1")
    if (!is.logical(stemWords)) 
        stop("stemWords must be logical")
    if (length(stemWords) != 1) 
        stop("stemWords must be length 1")
    if (!is.logical(rmStopWords) & !is.character(rmStopWords)) 
        stop("rmStopWords must be a logical or a character vector")
    if (is.character(rmStopWords)) {
        rmStopWordFlag <- TRUE
        stopwords <- rmStopWords
    }
    else if (is.logical(rmStopWords)) {
        if (length(rmStopWords) != 1) 
            stop("rmStopWords must be length 1 if passed as a logical")
        if (rmStopWords) {
            rmStopWordFlag <- TRUE
            stopwords <- smart_stopwords
        }
        else {
            rmStopWordFlag <- FALSE
        }
    }

    if (removeNum) 
        text <- gsub(x = text, pattern = "([[:digit:]])", replacement = "")
    if (toLower) 
        text <- tolower(text)
    text <- trimws(gsub(x = text, pattern = "\\s+", replacement = " "))
    text <- strsplit(x = text, split = " ", fixed = TRUE)
    text <- list(unlist(unique(text[[1]])))
    if (rmStopWordFlag) 
        text <- lapply(text, function(tokens) {
            checkTokens <- tolower(tokens)
            if (!removePunc) {
                checkTokens <- gsub(x = checkTokens, pattern = "[^[:alnum:] ]", 
                  replacement = "")
            }
            nonStopTok <- tokens[which(!checkTokens %in% stopwords)]
            if (length(nonStopTok) == 0) 
                NA_character_
            else nonStopTok
        })
    if (stemWords) {
        text <- lapply(text, function(w) {
            w_na = which(is.na(w))
            out = SnowballC::wordStem(w)
            out[w_na] = NA
            out
        })
    }
    tokenList <- lapply(text, function(tokens) {
        goodTok <- tokens[which(trimws(tokens) != "")]
        if (length(goodTok) == 0) 
            NA_character_
        else goodTok
    })
    tokenList
}

my_sentence_parser <- function (text) 
{
    strsplit(x = text, split = "(?<!\\w\\.\\w.)(?<![A-Z][a-z]\\.)(?<=\\.|\\?|\\!)\\s", 
        perl = TRUE)
}

my_sentenceSimil <- function (sentenceId, token, docId = NULL, sentencesAsDocs = FALSE) 
{
    if (!is.logical(sentencesAsDocs)) 
        stop("sentencesAsDocs must be logical")
    if (length(sentencesAsDocs) != 1) 
        stop("sentencesAsDocs must be length 1")
    if (!sentencesAsDocs & is.null(docId)) 
        stop("docIds must be provided if sentencesAsDocs is FALSE")
    sentenceId <- as.character(sentenceId)
    if (!is.character(token)) 
        stop("token must be character")
    if (length(token) < 1) 
        stop("token must be at least length 1")
    if (sentencesAsDocs) {
        docId <- sentenceId
        if (length(docId) != length(sentenceId) | length(docId) != 
            length(token)) 
            stop("docId, sentenceId, & token must all be the same length")
    }
    else if (!sentencesAsDocs) {
        docId <- as.character(docId)
        if (length(sentenceId) != length(token)) 
            stop("sentenceId & token must be the same length")
    }
    ndoc <- length(unique(docId))
    if (ndoc > length(unique(sentenceId))) 
        warning("There are more unique docIds than sentenceIds.  Verify you have passed the correct parameters to the function.")
    tokenDf <- data.frame(docId = docId, sentenceId = sentenceId, 
    token = token, stringsAsFactors = FALSE)
    stmList = split(tokenDf, paste0(tokenDf$docId, tokenDf$token))
    stmList = lapply(stmList, function(dfi) {
        dfi[["tf"]] = nrow(dfi)
        unique(dfi)
    })
    stm = do.call("rbind", stmList)
    stmList = split(stm, stm$token)
    stmList = lapply(stmList, function(dfi) {
        # dfi[["idf"]] = 1 + log(ndoc/length(unique(dfi$docId)))
        dfi[["idf"]] = log(ndoc/length(unique(dfi$docId)))
        dfi[["tfidf"]] = dfi$tf * dfi$idf
        unique(dfi)
    })
    stm = do.call("rbind", stmList)
    rownames(stm) = NULL
    stm = stm[order(stm$docId, stm$token), c("docId", "token", 
        "tf", "idf", "tfidf")]
    if (!sentencesAsDocs) {
        stm = merge(tokenDf, stm, by = c("docId", "token"), all.x = FALSE, 
            all.y = TRUE)
        stm = unique(stm[stm$tfidf > 0, c("sentenceId", "token", 
            "tfidf")])
    }
    else if (sentencesAsDocs) {
        stm = unique(stm[stm$tfidf > 0, c("docId", "token", "tfidf")])
        names(stm) = c("sentenceId", "token", "tfidf")
    }
    stm = stm[order(stm$sentenceId, stm$token), ]
    if (nrow(stm) == 0) 
        stop("All values in sentence term tfidf matrix are 0.  Similarities would return as NaN")
    if (length(unique((stm$sentenceId))) == 1) 
        stop("Only one sentence had nonzero tfidf scores.  Similarities would return as NaN")
    stm = xtabs(tfidf ~ sentenceId + token, stm)
    sentencePairsDf = as.data.frame(t(combn(sort(rownames(stm)), 
        2)), stringsAsFactors = FALSE)
     sentencePairsDf[["similVal"]] = .Call("_lexRankr_idfCosineSimil", PACKAGE = "lexRankr", stm)
    names(sentencePairsDf) = c("sent1", "sent2", "similVal")
    return(sentencePairsDf)
}


#text = c("Testing the system. Second sentence for you.", "System testing the tidy documents df.",  "Documents will be parsed and lexranked.")

#a <- "バナナ バナナ バナナ りんご みかん さくらんぼ さくらんぼ"
#b <- "バナナ りんご みかん りんご ぶどう ぶどう さくらんぼ"
#c <- "バナナ りんご りんご みかん バナナ ぶどう"
#text = c(a, b, c)
