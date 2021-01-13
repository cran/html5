#' Generate HTML string with properly declared DOCTYPE.
#'
#' @param ... A string or strings of HTML element tags.
#' @param doctype A string declaring the DOCTYPE for the HTML content.
#' @return A HTML document string.
html_doc <- function(..., doctype = "html"){
  return(paste0("<!DOCTYPE ", doctype, ">", ...))  
}

#' Generate HTML tag for element a
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param download A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param href A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hreflang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param ping A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param referrerpolicy A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param rel A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param target A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param type A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' a(class = "test", "Example")
a <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
download = NULL,
draggable = NULL,
hidden = NULL,
href = NULL,
hreflang = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
ping = NULL,
referrerpolicy = NULL,
rel = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
target = NULL,
title = NULL,
type = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "download" = download, "draggable" = draggable, "hidden" = hidden, "href" = href, "hreflang" = hreflang, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "ping" = ping, "referrerpolicy" = referrerpolicy, "rel" = rel, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "target" = target, "title" = title, "type" = type, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<a ", attrs, ">")
x <- paste0(s_tag, ..., "</a>")
}else{
x <- paste0("<a>", ..., "</a>")
}
return(x)
}

#' Generate HTML tag for element abbr
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' abbr(class = "test", "Example")
abbr <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<abbr ", attrs, ">")
x <- paste0(s_tag, ..., "</abbr>")
}else{
x <- paste0("<abbr>", ..., "</abbr>")
}
return(x)
}

#' Generate HTML tag for element address
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' address(class = "test", "Example")
address <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<address ", attrs, ">")
x <- paste0(s_tag, ..., "</address>")
}else{
x <- paste0("<address>", ..., "</address>")
}
return(x)
}

#' Generate HTML tag for element area
#'
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param alt A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param coords A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param download A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param href A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hreflang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param ping A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param referrerpolicy A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param rel A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param shape A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param target A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' area(class = "test")
area <- function(
accesskey = NULL,
alt = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
coords = NULL,
dir = NULL,
download = NULL,
draggable = NULL,
hidden = NULL,
href = NULL,
hreflang = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
ping = NULL,
referrerpolicy = NULL,
rel = NULL,
shape = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
target = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "alt" = alt, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "coords" = coords, "dir" = dir, "download" = download, "draggable" = draggable, "hidden" = hidden, "href" = href, "hreflang" = hreflang, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "ping" = ping, "referrerpolicy" = referrerpolicy, "rel" = rel, "shape" = shape, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "target" = target, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<area ", attrs, ">")
x <- s_tag
}else{
x <- "<area>"
}
return(x)
}

#' Generate HTML tag for element article
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' article(class = "test", "Example")
article <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<article ", attrs, ">")
x <- paste0(s_tag, ..., "</article>")
}else{
x <- paste0("<article>", ..., "</article>")
}
return(x)
}

#' Generate HTML tag for element aside
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' aside(class = "test", "Example")
aside <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<aside ", attrs, ">")
x <- paste0(s_tag, ..., "</aside>")
}else{
x <- paste0("<aside>", ..., "</aside>")
}
return(x)
}

#' Generate HTML tag for element audio
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autoplay A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param controls A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param crossorigin A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param currentTime A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param disableRemotePlayback A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param duration A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param loop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param muted A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param preload A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param src A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' audio(class = "test", "Example")
audio <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
autoplay = NULL,
class = NULL,
contenteditable = NULL,
controls = NULL,
crossorigin = NULL,
currentTime = NULL,
dir = NULL,
disableRemotePlayback = NULL,
draggable = NULL,
duration = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
loop = NULL,
muted = NULL,
part = NULL,
preload = NULL,
slot = NULL,
spellcheck = NULL,
src = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "autoplay" = autoplay, "class" = class, "contenteditable" = contenteditable, "controls" = controls, "crossorigin" = crossorigin, "currentTime" = currentTime, "dir" = dir, "disableRemotePlayback" = disableRemotePlayback, "draggable" = draggable, "duration" = duration, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "loop" = loop, "muted" = muted, "part" = part, "preload" = preload, "slot" = slot, "spellcheck" = spellcheck, "src" = src, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<audio ", attrs, ">")
x <- paste0(s_tag, ..., "</audio>")
}else{
x <- paste0("<audio>", ..., "</audio>")
}
return(x)
}

#' Generate HTML tag for element b
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' b(class = "test", "Example")
b <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<b ", attrs, ">")
x <- paste0(s_tag, ..., "</b>")
}else{
x <- paste0("<b>", ..., "</b>")
}
return(x)
}

#' Generate HTML tag for element base
#'
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param href A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param target A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' base(class = "test")
base <- function(
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
href = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
target = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "href" = href, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "target" = target, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<base ", attrs, ">")
x <- s_tag
}else{
x <- "<base>"
}
return(x)
}

#' Generate HTML tag for element bdi
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' bdi(class = "test", "Example")
bdi <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<bdi ", attrs, ">")
x <- paste0(s_tag, ..., "</bdi>")
}else{
x <- paste0("<bdi>", ..., "</bdi>")
}
return(x)
}

#' Generate HTML tag for element bdo
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' bdo(class = "test", "Example")
bdo <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<bdo ", attrs, ">")
x <- paste0(s_tag, ..., "</bdo>")
}else{
x <- paste0("<bdo>", ..., "</bdo>")
}
return(x)
}

#' Generate HTML tag for element blockquote
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param cite A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' blockquote(class = "test", "Example")
blockquote <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
cite = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "cite" = cite, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<blockquote ", attrs, ">")
x <- paste0(s_tag, ..., "</blockquote>")
}else{
x <- paste0("<blockquote>", ..., "</blockquote>")
}
return(x)
}

#' Generate HTML tag for element body
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param onafterprint A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param onbeforeprint A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param onbeforeunload A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param onblur A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param onerror A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param onfocus A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param onhashchange A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param onlanguagechange A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param onload A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param onmessage A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param onoffline A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param ononline A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param onpopstate A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param onredo A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param onresize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param onstorage A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param onundo A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param onunload A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' body(class = "test", "Example")
body <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
onafterprint = NULL,
onbeforeprint = NULL,
onbeforeunload = NULL,
onblur = NULL,
onerror = NULL,
onfocus = NULL,
onhashchange = NULL,
onlanguagechange = NULL,
onload = NULL,
onmessage = NULL,
onoffline = NULL,
ononline = NULL,
onpopstate = NULL,
onredo = NULL,
onresize = NULL,
onstorage = NULL,
onundo = NULL,
onunload = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "onafterprint" = onafterprint, "onbeforeprint" = onbeforeprint, "onbeforeunload" = onbeforeunload, "onblur" = onblur, "onerror" = onerror, "onfocus" = onfocus, "onhashchange" = onhashchange, "onlanguagechange" = onlanguagechange, "onload" = onload, "onmessage" = onmessage, "onoffline" = onoffline, "ononline" = ononline, "onpopstate" = onpopstate, "onredo" = onredo, "onresize" = onresize, "onstorage" = onstorage, "onundo" = onundo, "onunload" = onunload, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<body ", attrs, ">")
x <- paste0(s_tag, ..., "</body>")
}else{
x <- paste0("<body>", ..., "</body>")
}
return(x)
}

#' Generate HTML tag for element br
#'
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param clear A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' br(class = "test")
br <- function(
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
clear = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "clear" = clear, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<br ", attrs, ">")
x <- s_tag
}else{
x <- "<br>"
}
return(x)
}

#' Generate HTML tag for element button
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autofocus A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param disabled A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param form A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param formaction A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param formenctype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param formmethod A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param formnovalidate A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param formtarget A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param name A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param type A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param value A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' button(class = "test", "Example")
button <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
autofocus = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
disabled = NULL,
draggable = NULL,
form = NULL,
formaction = NULL,
formenctype = NULL,
formmethod = NULL,
formnovalidate = NULL,
formtarget = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
name = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
type = NULL,
value = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "autofocus" = autofocus, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "disabled" = disabled, "draggable" = draggable, "form" = form, "formaction" = formaction, "formenctype" = formenctype, "formmethod" = formmethod, "formnovalidate" = formnovalidate, "formtarget" = formtarget, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "name" = name, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, "type" = type, "value" = value, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<button ", attrs, ">")
x <- paste0(s_tag, ..., "</button>")
}else{
x <- paste0("<button>", ..., "</button>")
}
return(x)
}

#' Generate HTML tag for element canvas
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param height A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param width A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' canvas(class = "test", "Example")
canvas <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
height = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
width = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "height" = height, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, "width" = width, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<canvas ", attrs, ">")
x <- paste0(s_tag, ..., "</canvas>")
}else{
x <- paste0("<canvas>", ..., "</canvas>")
}
return(x)
}

#' Generate HTML tag for element caption
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' caption(class = "test", "Example")
caption <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<caption ", attrs, ">")
x <- paste0(s_tag, ..., "</caption>")
}else{
x <- paste0("<caption>", ..., "</caption>")
}
return(x)
}

#' Generate HTML tag for element cite
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' cite(class = "test", "Example")
cite <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<cite ", attrs, ">")
x <- paste0(s_tag, ..., "</cite>")
}else{
x <- paste0("<cite>", ..., "</cite>")
}
return(x)
}

#' Generate HTML tag for element code
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' code(class = "test", "Example")
code <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<code ", attrs, ">")
x <- paste0(s_tag, ..., "</code>")
}else{
x <- paste0("<code>", ..., "</code>")
}
return(x)
}

#' Generate HTML tag for element col
#'
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param span A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' col(class = "test")
col <- function(
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
span = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "span" = span, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<col ", attrs, ">")
x <- s_tag
}else{
x <- "<col>"
}
return(x)
}

#' Generate HTML tag for element colgroup
#'
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param span A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' colgroup(class = "test")
colgroup <- function(
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
span = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "span" = span, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<colgroup ", attrs, ">")
x <- s_tag
}else{
x <- "<colgroup>"
}
return(x)
}

#' Generate HTML tag for element data
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param value A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' data(class = "test", "Example")
data <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
value = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, "value" = value, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<data ", attrs, ">")
x <- paste0(s_tag, ..., "</data>")
}else{
x <- paste0("<data>", ..., "</data>")
}
return(x)
}

#' Generate HTML tag for element datalist
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' datalist(class = "test", "Example")
datalist <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<datalist ", attrs, ">")
x <- paste0(s_tag, ..., "</datalist>")
}else{
x <- paste0("<datalist>", ..., "</datalist>")
}
return(x)
}

#' Generate HTML tag for element dd
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' dd(class = "test", "Example")
dd <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<dd ", attrs, ">")
x <- paste0(s_tag, ..., "</dd>")
}else{
x <- paste0("<dd>", ..., "</dd>")
}
return(x)
}

#' Generate HTML tag for element del
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param cite A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param datetime A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' del(class = "test", "Example")
del <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
cite = NULL,
class = NULL,
contenteditable = NULL,
datetime = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "cite" = cite, "class" = class, "contenteditable" = contenteditable, "datetime" = datetime, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<del ", attrs, ">")
x <- paste0(s_tag, ..., "</del>")
}else{
x <- paste0("<del>", ..., "</del>")
}
return(x)
}

#' Generate HTML tag for element details
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param open A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' details(class = "test", "Example")
details <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
open = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "open" = open, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<details ", attrs, ">")
x <- paste0(s_tag, ..., "</details>")
}else{
x <- paste0("<details>", ..., "</details>")
}
return(x)
}

#' Generate HTML tag for element dfn
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' dfn(class = "test", "Example")
dfn <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<dfn ", attrs, ">")
x <- paste0(s_tag, ..., "</dfn>")
}else{
x <- paste0("<dfn>", ..., "</dfn>")
}
return(x)
}

#' Generate HTML tag for element dialog
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param open A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' dialog(class = "test", "Example")
dialog <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
open = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "open" = open, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<dialog ", attrs, ">")
x <- paste0(s_tag, ..., "</dialog>")
}else{
x <- paste0("<dialog>", ..., "</dialog>")
}
return(x)
}

#' Generate HTML tag for element div
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' div(class = "test", "Example")
div <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<div ", attrs, ">")
x <- paste0(s_tag, ..., "</div>")
}else{
x <- paste0("<div>", ..., "</div>")
}
return(x)
}

#' Generate HTML tag for element dl
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' dl(class = "test", "Example")
dl <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<dl ", attrs, ">")
x <- paste0(s_tag, ..., "</dl>")
}else{
x <- paste0("<dl>", ..., "</dl>")
}
return(x)
}

#' Generate HTML tag for element dt
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' dt(class = "test", "Example")
dt <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<dt ", attrs, ">")
x <- paste0(s_tag, ..., "</dt>")
}else{
x <- paste0("<dt>", ..., "</dt>")
}
return(x)
}

#' Generate HTML tag for element em
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' em(class = "test", "Example")
em <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<em ", attrs, ">")
x <- paste0(s_tag, ..., "</em>")
}else{
x <- paste0("<em>", ..., "</em>")
}
return(x)
}

#' Generate HTML tag for element embed
#'
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param height A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param src A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param type A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param width A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' embed(class = "test")
embed <- function(
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
height = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
src = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
type = NULL,
width = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "height" = height, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "src" = src, "style" = style, "tabindex" = tabindex, "title" = title, "type" = type, "width" = width, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<embed ", attrs, ">")
x <- s_tag
}else{
x <- "<embed>"
}
return(x)
}

#' Generate HTML tag for element fieldset
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param disabled A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param form A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param name A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' fieldset(class = "test", "Example")
fieldset <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
disabled = NULL,
draggable = NULL,
form = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
name = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "disabled" = disabled, "draggable" = draggable, "form" = form, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "name" = name, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<fieldset ", attrs, ">")
x <- paste0(s_tag, ..., "</fieldset>")
}else{
x <- paste0("<fieldset>", ..., "</fieldset>")
}
return(x)
}

#' Generate HTML tag for element figcaption
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' figcaption(class = "test", "Example")
figcaption <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<figcaption ", attrs, ">")
x <- paste0(s_tag, ..., "</figcaption>")
}else{
x <- paste0("<figcaption>", ..., "</figcaption>")
}
return(x)
}

#' Generate HTML tag for element figure
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' figure(class = "test", "Example")
figure <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<figure ", attrs, ">")
x <- paste0(s_tag, ..., "</figure>")
}else{
x <- paste0("<figure>", ..., "</figure>")
}
return(x)
}

#' Generate HTML tag for element footer
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' footer(class = "test", "Example")
footer <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<footer ", attrs, ">")
x <- paste0(s_tag, ..., "</footer>")
}else{
x <- paste0("<footer>", ..., "</footer>")
}
return(x)
}

#' Generate HTML tag for element form
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accept_charset A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param action A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocomplete A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param enctype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param method A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param name A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param novalidate A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param rel A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param target A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' form(class = "test", "Example")
form <- function(
...,
accept_charset = NULL,
accesskey = NULL,
action = NULL,
autocapitalize = NULL,
autocomplete = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
enctype = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
method = NULL,
name = NULL,
novalidate = NULL,
part = NULL,
rel = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
target = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accept-charset" = accept_charset, "accesskey" = accesskey, "action" = action, "autocapitalize" = autocapitalize, "autocomplete" = autocomplete, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "enctype" = enctype, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "method" = method, "name" = name, "novalidate" = novalidate, "part" = part, "rel" = rel, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "target" = target, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<form ", attrs, ">")
x <- paste0(s_tag, ..., "</form>")
}else{
x <- paste0("<form>", ..., "</form>")
}
return(x)
}

#' Generate HTML tag for element h1
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' h1(class = "test", "Example")
h1 <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<h1 ", attrs, ">")
x <- paste0(s_tag, ..., "</h1>")
}else{
x <- paste0("<h1>", ..., "</h1>")
}
return(x)
}

#' Generate HTML tag for element h2
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' h2(class = "test", "Example")
h2 <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<h2 ", attrs, ">")
x <- paste0(s_tag, ..., "</h2>")
}else{
x <- paste0("<h2>", ..., "</h2>")
}
return(x)
}

#' Generate HTML tag for element h3
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' h3(class = "test", "Example")
h3 <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<h3 ", attrs, ">")
x <- paste0(s_tag, ..., "</h3>")
}else{
x <- paste0("<h3>", ..., "</h3>")
}
return(x)
}

#' Generate HTML tag for element h4
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' h4(class = "test", "Example")
h4 <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<h4 ", attrs, ">")
x <- paste0(s_tag, ..., "</h4>")
}else{
x <- paste0("<h4>", ..., "</h4>")
}
return(x)
}

#' Generate HTML tag for element h5
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' h5(class = "test", "Example")
h5 <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<h5 ", attrs, ">")
x <- paste0(s_tag, ..., "</h5>")
}else{
x <- paste0("<h5>", ..., "</h5>")
}
return(x)
}

#' Generate HTML tag for element h6
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' h6(class = "test", "Example")
h6 <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<h6 ", attrs, ">")
x <- paste0(s_tag, ..., "</h6>")
}else{
x <- paste0("<h6>", ..., "</h6>")
}
return(x)
}

#' Generate HTML tag for element head
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' head(class = "test", "Example")
head <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<head ", attrs, ">")
x <- paste0(s_tag, ..., "</head>")
}else{
x <- paste0("<head>", ..., "</head>")
}
return(x)
}

#' Generate HTML tag for element header
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' header(class = "test", "Example")
header <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<header ", attrs, ">")
x <- paste0(s_tag, ..., "</header>")
}else{
x <- paste0("<header>", ..., "</header>")
}
return(x)
}

#' Generate HTML tag for element hgroup
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' hgroup(class = "test", "Example")
hgroup <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<hgroup ", attrs, ">")
x <- paste0(s_tag, ..., "</hgroup>")
}else{
x <- paste0("<hgroup>", ..., "</hgroup>")
}
return(x)
}

#' Generate HTML tag for element hr
#'
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' hr(class = "test")
hr <- function(
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<hr ", attrs, ">")
x <- s_tag
}else{
x <- "<hr>"
}
return(x)
}

#' Generate HTML tag for element html
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param xmlns A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' html(class = "test", "Example")
html <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
xmlns = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, "xmlns" = xmlns, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<html ", attrs, ">")
x <- paste0(s_tag, ..., "</html>")
}else{
x <- paste0("<html>", ..., "</html>")
}
return(x)
}

#' Generate HTML tag for element i
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' i(class = "test", "Example")
i <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<i ", attrs, ">")
x <- paste0(s_tag, ..., "</i>")
}else{
x <- paste0("<i>", ..., "</i>")
}
return(x)
}

#' Generate HTML tag for element iframe
#'
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param allow A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param allowfullscreen A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param allowpaymentrequest A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param csp A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param height A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param loading A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param name A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param referrerpolicy A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param sandbox A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param src A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param srcdoc A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param width A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' iframe(class = "test")
iframe <- function(
accesskey = NULL,
allow = NULL,
allowfullscreen = NULL,
allowpaymentrequest = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
csp = NULL,
dir = NULL,
draggable = NULL,
height = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
loading = NULL,
name = NULL,
part = NULL,
referrerpolicy = NULL,
sandbox = NULL,
slot = NULL,
spellcheck = NULL,
src = NULL,
srcdoc = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
width = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "allow" = allow, "allowfullscreen" = allowfullscreen, "allowpaymentrequest" = allowpaymentrequest, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "csp" = csp, "dir" = dir, "draggable" = draggable, "height" = height, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "loading" = loading, "name" = name, "part" = part, "referrerpolicy" = referrerpolicy, "sandbox" = sandbox, "slot" = slot, "spellcheck" = spellcheck, "src" = src, "srcdoc" = srcdoc, "style" = style, "tabindex" = tabindex, "title" = title, "width" = width, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<iframe ", attrs, ">")
x <- s_tag
}else{
x <- "<iframe>"
}
return(x)
}

#' Generate HTML tag for element img
#'
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param alt A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param crossorigin A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param decoding A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param height A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param ismap A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param loading A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param referrerpolicy A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param sizes A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param src A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param srcset A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param usemap A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param width A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' img(class = "test")
img <- function(
accesskey = NULL,
alt = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
crossorigin = NULL,
decoding = NULL,
dir = NULL,
draggable = NULL,
height = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
ismap = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
loading = NULL,
part = NULL,
referrerpolicy = NULL,
sizes = NULL,
slot = NULL,
spellcheck = NULL,
src = NULL,
srcset = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
usemap = NULL,
width = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "alt" = alt, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "crossorigin" = crossorigin, "decoding" = decoding, "dir" = dir, "draggable" = draggable, "height" = height, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "ismap" = ismap, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "loading" = loading, "part" = part, "referrerpolicy" = referrerpolicy, "sizes" = sizes, "slot" = slot, "spellcheck" = spellcheck, "src" = src, "srcset" = srcset, "style" = style, "tabindex" = tabindex, "title" = title, "usemap" = usemap, "width" = width, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<img ", attrs, ">")
x <- s_tag
}else{
x <- "<img>"
}
return(x)
}

#' Generate HTML tag for element input
#'
#' @param accept A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param alt A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocomplete A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autofocus A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param capture A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param checked A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dirname A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param disabled A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param form A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param formaction A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param formenctype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param formmethod A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param formnovalidate A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param formtarget A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param height A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param list A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param max A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param maxlength A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param min A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param minlength A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param multiple A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param name A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param pattern A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param placeholder A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param readonly A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param required A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param size A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param src A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param step A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param type A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param value A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param width A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' input(class = "test")
input <- function(
accept = NULL,
accesskey = NULL,
alt = NULL,
autocapitalize = NULL,
autocomplete = NULL,
autofocus = NULL,
capture = NULL,
checked = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
dirname = NULL,
disabled = NULL,
draggable = NULL,
form = NULL,
formaction = NULL,
formenctype = NULL,
formmethod = NULL,
formnovalidate = NULL,
formtarget = NULL,
height = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
list = NULL,
max = NULL,
maxlength = NULL,
min = NULL,
minlength = NULL,
multiple = NULL,
name = NULL,
part = NULL,
pattern = NULL,
placeholder = NULL,
readonly = NULL,
required = NULL,
size = NULL,
slot = NULL,
spellcheck = NULL,
src = NULL,
step = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
type = NULL,
value = NULL,
width = NULL,
custom_attr = NULL){
attr_values <- c("accept" = accept, "accesskey" = accesskey, "alt" = alt, "autocapitalize" = autocapitalize, "autocomplete" = autocomplete, "autofocus" = autofocus, "capture" = capture, "checked" = checked, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "dirname" = dirname, "disabled" = disabled, "draggable" = draggable, "form" = form, "formaction" = formaction, "formenctype" = formenctype, "formmethod" = formmethod, "formnovalidate" = formnovalidate, "formtarget" = formtarget, "height" = height, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "list" = list, "max" = max, "maxlength" = maxlength, "min" = min, "minlength" = minlength, "multiple" = multiple, "name" = name, "part" = part, "pattern" = pattern, "placeholder" = placeholder, "readonly" = readonly, "required" = required, "size" = size, "slot" = slot, "spellcheck" = spellcheck, "src" = src, "step" = step, "style" = style, "tabindex" = tabindex, "title" = title, "type" = type, "value" = value, "width" = width, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<input ", attrs, ">")
x <- s_tag
}else{
x <- "<input>"
}
return(x)
}

#' Generate HTML tag for element ins
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param cite A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param datetime A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' ins(class = "test", "Example")
ins <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
cite = NULL,
class = NULL,
contenteditable = NULL,
datetime = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "cite" = cite, "class" = class, "contenteditable" = contenteditable, "datetime" = datetime, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<ins ", attrs, ">")
x <- paste0(s_tag, ..., "</ins>")
}else{
x <- paste0("<ins>", ..., "</ins>")
}
return(x)
}

#' Generate HTML tag for element kbd
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' kbd(class = "test", "Example")
kbd <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<kbd ", attrs, ">")
x <- paste0(s_tag, ..., "</kbd>")
}else{
x <- paste0("<kbd>", ..., "</kbd>")
}
return(x)
}

#' Generate HTML tag for element label
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param for_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' label(class = "test", "Example")
label <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
for_attr = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "for" = for_attr, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<label ", attrs, ">")
x <- paste0(s_tag, ..., "</label>")
}else{
x <- paste0("<label>", ..., "</label>")
}
return(x)
}

#' Generate HTML tag for element legend
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' legend(class = "test", "Example")
legend <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<legend ", attrs, ">")
x <- paste0(s_tag, ..., "</legend>")
}else{
x <- paste0("<legend>", ..., "</legend>")
}
return(x)
}

#' Generate HTML tag for element li
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param value A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' li(class = "test", "Example")
li <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
value = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, "value" = value, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<li ", attrs, ">")
x <- paste0(s_tag, ..., "</li>")
}else{
x <- paste0("<li>", ..., "</li>")
}
return(x)
}

#' Generate HTML tag for element link
#'
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param as A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param crossorigin A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param disabled A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param href A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hreflang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param imagesizes A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param imagesrcset A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param integrity A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param media A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param prefetch A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param referrerpolicy A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param rel A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param sizes A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param type A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' link(class = "test")
link <- function(
accesskey = NULL,
as = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
crossorigin = NULL,
dir = NULL,
disabled = NULL,
draggable = NULL,
hidden = NULL,
href = NULL,
hreflang = NULL,
id = NULL,
imagesizes = NULL,
imagesrcset = NULL,
inputmode = NULL,
integrity = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
media = NULL,
part = NULL,
prefetch = NULL,
referrerpolicy = NULL,
rel = NULL,
sizes = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
type = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "as" = as, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "crossorigin" = crossorigin, "dir" = dir, "disabled" = disabled, "draggable" = draggable, "hidden" = hidden, "href" = href, "hreflang" = hreflang, "id" = id, "imagesizes" = imagesizes, "imagesrcset" = imagesrcset, "inputmode" = inputmode, "integrity" = integrity, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "media" = media, "part" = part, "prefetch" = prefetch, "referrerpolicy" = referrerpolicy, "rel" = rel, "sizes" = sizes, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, "type" = type, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<link ", attrs, ">")
x <- s_tag
}else{
x <- "<link>"
}
return(x)
}

#' Generate HTML tag for element main
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' main(class = "test", "Example")
main <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<main ", attrs, ">")
x <- paste0(s_tag, ..., "</main>")
}else{
x <- paste0("<main>", ..., "</main>")
}
return(x)
}

#' Generate HTML tag for element map
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param name A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' map(class = "test", "Example")
map <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
name = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "name" = name, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<map ", attrs, ">")
x <- paste0(s_tag, ..., "</map>")
}else{
x <- paste0("<map>", ..., "</map>")
}
return(x)
}

#' Generate HTML tag for element mark
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' mark(class = "test", "Example")
mark <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<mark ", attrs, ">")
x <- paste0(s_tag, ..., "</mark>")
}else{
x <- paste0("<mark>", ..., "</mark>")
}
return(x)
}

#' Generate HTML tag for element menu
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param type A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' menu(class = "test", "Example")
menu <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
type = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, "type" = type, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<menu ", attrs, ">")
x <- paste0(s_tag, ..., "</menu>")
}else{
x <- paste0("<menu>", ..., "</menu>")
}
return(x)
}

#' Generate HTML tag for element meta
#'
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param charset A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param content A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param http_equiv A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param name A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' meta(class = "test")
meta <- function(
accesskey = NULL,
autocapitalize = NULL,
charset = NULL,
class = NULL,
content = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
http_equiv = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
name = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "charset" = charset, "class" = class, "content" = content, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "http-equiv" = http_equiv, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "name" = name, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<meta ", attrs, ">")
x <- s_tag
}else{
x <- "<meta>"
}
return(x)
}

#' Generate HTML tag for element meter
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param form A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param high A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param low A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param max A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param min A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param optimum A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param value A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' meter(class = "test", "Example")
meter <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
form = NULL,
hidden = NULL,
high = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
low = NULL,
max = NULL,
min = NULL,
optimum = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
value = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "form" = form, "hidden" = hidden, "high" = high, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "low" = low, "max" = max, "min" = min, "optimum" = optimum, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, "value" = value, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<meter ", attrs, ">")
x <- paste0(s_tag, ..., "</meter>")
}else{
x <- paste0("<meter>", ..., "</meter>")
}
return(x)
}

#' Generate HTML tag for element nav
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' nav(class = "test", "Example")
nav <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<nav ", attrs, ">")
x <- paste0(s_tag, ..., "</nav>")
}else{
x <- paste0("<nav>", ..., "</nav>")
}
return(x)
}

#' Generate HTML tag for element noscript
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' noscript(class = "test", "Example")
noscript <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<noscript ", attrs, ">")
x <- paste0(s_tag, ..., "</noscript>")
}else{
x <- paste0("<noscript>", ..., "</noscript>")
}
return(x)
}

#' Generate HTML tag for element object
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param data A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param form A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param height A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param name A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param type A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param typemustmatch A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param usemap A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param width A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' object(class = "test", "Example")
object <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
data = NULL,
dir = NULL,
draggable = NULL,
form = NULL,
height = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
name = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
type = NULL,
typemustmatch = NULL,
usemap = NULL,
width = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "data" = data, "dir" = dir, "draggable" = draggable, "form" = form, "height" = height, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "name" = name, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, "type" = type, "typemustmatch" = typemustmatch, "usemap" = usemap, "width" = width, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<object ", attrs, ">")
x <- paste0(s_tag, ..., "</object>")
}else{
x <- paste0("<object>", ..., "</object>")
}
return(x)
}

#' Generate HTML tag for element ol
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param reversed A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param start A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param type A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' ol(class = "test", "Example")
ol <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
reversed = NULL,
slot = NULL,
spellcheck = NULL,
start = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
type = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "reversed" = reversed, "slot" = slot, "spellcheck" = spellcheck, "start" = start, "style" = style, "tabindex" = tabindex, "title" = title, "type" = type, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<ol ", attrs, ">")
x <- paste0(s_tag, ..., "</ol>")
}else{
x <- paste0("<ol>", ..., "</ol>")
}
return(x)
}

#' Generate HTML tag for element optgroup
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param disabled A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param label A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' optgroup(class = "test", "Example")
optgroup <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
disabled = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
label = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "disabled" = disabled, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "label" = label, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<optgroup ", attrs, ">")
x <- paste0(s_tag, ..., "</optgroup>")
}else{
x <- paste0("<optgroup>", ..., "</optgroup>")
}
return(x)
}

#' Generate HTML tag for element option
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param disabled A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param label A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param selected A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param value A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' option(class = "test", "Example")
option <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
disabled = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
label = NULL,
lang = NULL,
part = NULL,
selected = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
value = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "disabled" = disabled, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "label" = label, "lang" = lang, "part" = part, "selected" = selected, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, "value" = value, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<option ", attrs, ">")
x <- paste0(s_tag, ..., "</option>")
}else{
x <- paste0("<option>", ..., "</option>")
}
return(x)
}

#' Generate HTML tag for element output
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param for_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param form A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param name A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' output(class = "test", "Example")
output <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
for_attr = NULL,
form = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
name = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "for" = for_attr, "form" = form, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "name" = name, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<output ", attrs, ">")
x <- paste0(s_tag, ..., "</output>")
}else{
x <- paste0("<output>", ..., "</output>")
}
return(x)
}

#' Generate HTML tag for element p
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' p(class = "test", "Example")
p <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<p ", attrs, ">")
x <- paste0(s_tag, ..., "</p>")
}else{
x <- paste0("<p>", ..., "</p>")
}
return(x)
}

#' Generate HTML tag for element param
#'
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param name A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param value A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' param(class = "test")
param <- function(
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
name = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
value = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "name" = name, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, "value" = value, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<param ", attrs, ">")
x <- s_tag
}else{
x <- "<param>"
}
return(x)
}

#' Generate HTML tag for element picture
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' picture(class = "test", "Example")
picture <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<picture ", attrs, ">")
x <- paste0(s_tag, ..., "</picture>")
}else{
x <- paste0("<picture>", ..., "</picture>")
}
return(x)
}

#' Generate HTML tag for element pre
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' pre(class = "test", "Example")
pre <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<pre ", attrs, ">")
x <- paste0(s_tag, ..., "</pre>")
}else{
x <- paste0("<pre>", ..., "</pre>")
}
return(x)
}

#' Generate HTML tag for element progress
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param max A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param value A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' progress(class = "test", "Example")
progress <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
max = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
value = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "max" = max, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, "value" = value, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<progress ", attrs, ">")
x <- paste0(s_tag, ..., "</progress>")
}else{
x <- paste0("<progress>", ..., "</progress>")
}
return(x)
}

#' Generate HTML tag for element q
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param cite A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' q(class = "test", "Example")
q <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
cite = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "cite" = cite, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<q ", attrs, ">")
x <- paste0(s_tag, ..., "</q>")
}else{
x <- paste0("<q>", ..., "</q>")
}
return(x)
}

#' Generate HTML tag for element rb
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' rb(class = "test", "Example")
rb <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<rb ", attrs, ">")
x <- paste0(s_tag, ..., "</rb>")
}else{
x <- paste0("<rb>", ..., "</rb>")
}
return(x)
}

#' Generate HTML tag for element rp
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' rp(class = "test", "Example")
rp <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<rp ", attrs, ">")
x <- paste0(s_tag, ..., "</rp>")
}else{
x <- paste0("<rp>", ..., "</rp>")
}
return(x)
}

#' Generate HTML tag for element rt
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' rt(class = "test", "Example")
rt <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<rt ", attrs, ">")
x <- paste0(s_tag, ..., "</rt>")
}else{
x <- paste0("<rt>", ..., "</rt>")
}
return(x)
}

#' Generate HTML tag for element rtc
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' rtc(class = "test", "Example")
rtc <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<rtc ", attrs, ">")
x <- paste0(s_tag, ..., "</rtc>")
}else{
x <- paste0("<rtc>", ..., "</rtc>")
}
return(x)
}

#' Generate HTML tag for element ruby
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' ruby(class = "test", "Example")
ruby <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<ruby ", attrs, ">")
x <- paste0(s_tag, ..., "</ruby>")
}else{
x <- paste0("<ruby>", ..., "</ruby>")
}
return(x)
}

#' Generate HTML tag for element s
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' s(class = "test", "Example")
s <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<s ", attrs, ">")
x <- paste0(s_tag, ..., "</s>")
}else{
x <- paste0("<s>", ..., "</s>")
}
return(x)
}

#' Generate HTML tag for element samp
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' samp(class = "test", "Example")
samp <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<samp ", attrs, ">")
x <- paste0(s_tag, ..., "</samp>")
}else{
x <- paste0("<samp>", ..., "</samp>")
}
return(x)
}

#' Generate HTML tag for element script
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param async A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param crossorigin A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param defer A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param integrity A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param nomodule A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param nonce A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param referrerpolicy A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param src A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param type A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' script(class = "test", "Example")
script <- function(
...,
accesskey = NULL,
async = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
crossorigin = NULL,
defer = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
integrity = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
nomodule = NULL,
nonce = NULL,
part = NULL,
referrerpolicy = NULL,
slot = NULL,
spellcheck = NULL,
src = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
type = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "async" = async, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "crossorigin" = crossorigin, "defer" = defer, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "integrity" = integrity, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "nomodule" = nomodule, "nonce" = nonce, "part" = part, "referrerpolicy" = referrerpolicy, "slot" = slot, "spellcheck" = spellcheck, "src" = src, "style" = style, "tabindex" = tabindex, "title" = title, "type" = type, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<script ", attrs, ">")
x <- paste0(s_tag, ..., "</script>")
}else{
x <- paste0("<script>", ..., "</script>")
}
return(x)
}

#' Generate HTML tag for element section
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' section(class = "test", "Example")
section <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<section ", attrs, ">")
x <- paste0(s_tag, ..., "</section>")
}else{
x <- paste0("<section>", ..., "</section>")
}
return(x)
}

#' Generate HTML tag for element select
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocomplete A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autofocus A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param disabled A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param form A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param multiple A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param name A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param required A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param size A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' select(class = "test", "Example")
select <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
autocomplete = NULL,
autofocus = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
disabled = NULL,
draggable = NULL,
form = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
multiple = NULL,
name = NULL,
part = NULL,
required = NULL,
size = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "autocomplete" = autocomplete, "autofocus" = autofocus, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "disabled" = disabled, "draggable" = draggable, "form" = form, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "multiple" = multiple, "name" = name, "part" = part, "required" = required, "size" = size, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<select ", attrs, ">")
x <- paste0(s_tag, ..., "</select>")
}else{
x <- paste0("<select>", ..., "</select>")
}
return(x)
}

#' Generate HTML tag for element slot
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param name A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' slot(class = "test", "Example")
slot <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
name = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "name" = name, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<slot ", attrs, ">")
x <- paste0(s_tag, ..., "</slot>")
}else{
x <- paste0("<slot>", ..., "</slot>")
}
return(x)
}

#' Generate HTML tag for element small
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' small(class = "test", "Example")
small <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<small ", attrs, ">")
x <- paste0(s_tag, ..., "</small>")
}else{
x <- paste0("<small>", ..., "</small>")
}
return(x)
}

#' Generate HTML tag for element source
#'
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param media A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param sizes A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param src A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param srcset A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param type A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' source(class = "test")
source <- function(
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
media = NULL,
part = NULL,
sizes = NULL,
slot = NULL,
spellcheck = NULL,
src = NULL,
srcset = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
type = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "media" = media, "part" = part, "sizes" = sizes, "slot" = slot, "spellcheck" = spellcheck, "src" = src, "srcset" = srcset, "style" = style, "tabindex" = tabindex, "title" = title, "type" = type, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<source ", attrs, ">")
x <- s_tag
}else{
x <- "<source>"
}
return(x)
}

#' Generate HTML tag for element span
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' span(class = "test", "Example")
span <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<span ", attrs, ">")
x <- paste0(s_tag, ..., "</span>")
}else{
x <- paste0("<span>", ..., "</span>")
}
return(x)
}

#' Generate HTML tag for element strong
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' strong(class = "test", "Example")
strong <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<strong ", attrs, ">")
x <- paste0(s_tag, ..., "</strong>")
}else{
x <- paste0("<strong>", ..., "</strong>")
}
return(x)
}

#' Generate HTML tag for element style
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param media A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param nonce A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param type A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' style(class = "test", "Example")
style <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
media = NULL,
nonce = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
type = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "media" = media, "nonce" = nonce, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, "type" = type, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<style ", attrs, ">")
x <- paste0(s_tag, ..., "</style>")
}else{
x <- paste0("<style>", ..., "</style>")
}
return(x)
}

#' Generate HTML tag for element sub
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' sub(class = "test", "Example")
sub <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<sub ", attrs, ">")
x <- paste0(s_tag, ..., "</sub>")
}else{
x <- paste0("<sub>", ..., "</sub>")
}
return(x)
}

#' Generate HTML tag for element summary
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' summary(class = "test", "Example")
summary <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<summary ", attrs, ">")
x <- paste0(s_tag, ..., "</summary>")
}else{
x <- paste0("<summary>", ..., "</summary>")
}
return(x)
}

#' Generate HTML tag for element sup
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' sup(class = "test", "Example")
sup <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<sup ", attrs, ">")
x <- paste0(s_tag, ..., "</sup>")
}else{
x <- paste0("<sup>", ..., "</sup>")
}
return(x)
}

#' Generate HTML tag for element table
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' table(class = "test", "Example")
table <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<table ", attrs, ">")
x <- paste0(s_tag, ..., "</table>")
}else{
x <- paste0("<table>", ..., "</table>")
}
return(x)
}

#' Generate HTML tag for element tbody
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' tbody(class = "test", "Example")
tbody <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<tbody ", attrs, ">")
x <- paste0(s_tag, ..., "</tbody>")
}else{
x <- paste0("<tbody>", ..., "</tbody>")
}
return(x)
}

#' Generate HTML tag for element td
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param colspan A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param headers A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param rowspan A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' td(class = "test", "Example")
td <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
colspan = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
headers = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
rowspan = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "colspan" = colspan, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "headers" = headers, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "rowspan" = rowspan, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<td ", attrs, ">")
x <- paste0(s_tag, ..., "</td>")
}else{
x <- paste0("<td>", ..., "</td>")
}
return(x)
}

#' Generate HTML tag for element template
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' template(class = "test", "Example")
template <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<template ", attrs, ">")
x <- paste0(s_tag, ..., "</template>")
}else{
x <- paste0("<template>", ..., "</template>")
}
return(x)
}

#' Generate HTML tag for element textarea
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocomplete A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autofocus A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param cols A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param disabled A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param form A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param maxlength A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param minlength A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param name A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param placeholder A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param readonly A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param required A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param rows A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param wrap A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' textarea(class = "test", "Example")
textarea <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
autocomplete = NULL,
autofocus = NULL,
class = NULL,
cols = NULL,
contenteditable = NULL,
dir = NULL,
disabled = NULL,
draggable = NULL,
form = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
maxlength = NULL,
minlength = NULL,
name = NULL,
part = NULL,
placeholder = NULL,
readonly = NULL,
required = NULL,
rows = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
wrap = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "autocomplete" = autocomplete, "autofocus" = autofocus, "class" = class, "cols" = cols, "contenteditable" = contenteditable, "dir" = dir, "disabled" = disabled, "draggable" = draggable, "form" = form, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "maxlength" = maxlength, "minlength" = minlength, "name" = name, "part" = part, "placeholder" = placeholder, "readonly" = readonly, "required" = required, "rows" = rows, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, "wrap" = wrap, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<textarea ", attrs, ">")
x <- paste0(s_tag, ..., "</textarea>")
}else{
x <- paste0("<textarea>", ..., "</textarea>")
}
return(x)
}

#' Generate HTML tag for element tfoot
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' tfoot(class = "test", "Example")
tfoot <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<tfoot ", attrs, ">")
x <- paste0(s_tag, ..., "</tfoot>")
}else{
x <- paste0("<tfoot>", ..., "</tfoot>")
}
return(x)
}

#' Generate HTML tag for element th
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param abbr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param colspan A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param headers A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param rowspan A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param scope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' th(class = "test", "Example")
th <- function(
...,
abbr = NULL,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
colspan = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
headers = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
rowspan = NULL,
scope = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("abbr" = abbr, "accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "colspan" = colspan, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "headers" = headers, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "rowspan" = rowspan, "scope" = scope, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<th ", attrs, ">")
x <- paste0(s_tag, ..., "</th>")
}else{
x <- paste0("<th>", ..., "</th>")
}
return(x)
}

#' Generate HTML tag for element thead
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' thead(class = "test", "Example")
thead <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<thead ", attrs, ">")
x <- paste0(s_tag, ..., "</thead>")
}else{
x <- paste0("<thead>", ..., "</thead>")
}
return(x)
}

#' Generate HTML tag for element time
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param datetime A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' time(class = "test", "Example")
time <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
datetime = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "datetime" = datetime, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<time ", attrs, ">")
x <- paste0(s_tag, ..., "</time>")
}else{
x <- paste0("<time>", ..., "</time>")
}
return(x)
}

#' Generate HTML tag for element title
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' title(class = "test", "Example")
title <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<title ", attrs, ">")
x <- paste0(s_tag, ..., "</title>")
}else{
x <- paste0("<title>", ..., "</title>")
}
return(x)
}

#' Generate HTML tag for element tr
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' tr(class = "test", "Example")
tr <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<tr ", attrs, ">")
x <- paste0(s_tag, ..., "</tr>")
}else{
x <- paste0("<tr>", ..., "</tr>")
}
return(x)
}

#' Generate HTML tag for element track
#'
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param default A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param kind A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param label A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param src A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param srclang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' track(class = "test")
track <- function(
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
default = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
kind = NULL,
label = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
src = NULL,
srclang = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "default" = default, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "kind" = kind, "label" = label, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "src" = src, "srclang" = srclang, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<track ", attrs, ">")
x <- s_tag
}else{
x <- "<track>"
}
return(x)
}

#' Generate HTML tag for element u
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' u(class = "test", "Example")
u <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<u ", attrs, ">")
x <- paste0(s_tag, ..., "</u>")
}else{
x <- paste0("<u>", ..., "</u>")
}
return(x)
}

#' Generate HTML tag for element ul
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' ul(class = "test", "Example")
ul <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<ul ", attrs, ">")
x <- paste0(s_tag, ..., "</ul>")
}else{
x <- paste0("<ul>", ..., "</ul>")
}
return(x)
}

#' Generate HTML tag for element var
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' var(class = "test", "Example")
var <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<var ", attrs, ">")
x <- paste0(s_tag, ..., "</var>")
}else{
x <- paste0("<var>", ..., "</var>")
}
return(x)
}

#' Generate HTML tag for element video
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autoPictureInPicture A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autoplay A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param buffered A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param controls A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param controlslist A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param crossorigin A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param currentTime A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param disablePictureInPicture A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param disableRemotePlayback A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param duration A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param height A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param intrinsicsize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param loop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param muted A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param playsinline A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param poster A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param preload A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param src A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param width A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' video(class = "test", "Example")
video <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
autoPictureInPicture = NULL,
autoplay = NULL,
buffered = NULL,
class = NULL,
contenteditable = NULL,
controls = NULL,
controlslist = NULL,
crossorigin = NULL,
currentTime = NULL,
dir = NULL,
disablePictureInPicture = NULL,
disableRemotePlayback = NULL,
draggable = NULL,
duration = NULL,
height = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
intrinsicsize = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
loop = NULL,
muted = NULL,
part = NULL,
playsinline = NULL,
poster = NULL,
preload = NULL,
slot = NULL,
spellcheck = NULL,
src = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
width = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "autoPictureInPicture" = autoPictureInPicture, "autoplay" = autoplay, "buffered" = buffered, "class" = class, "contenteditable" = contenteditable, "controls" = controls, "controlslist" = controlslist, "crossorigin" = crossorigin, "currentTime" = currentTime, "dir" = dir, "disablePictureInPicture" = disablePictureInPicture, "disableRemotePlayback" = disableRemotePlayback, "draggable" = draggable, "duration" = duration, "height" = height, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "intrinsicsize" = intrinsicsize, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "loop" = loop, "muted" = muted, "part" = part, "playsinline" = playsinline, "poster" = poster, "preload" = preload, "slot" = slot, "spellcheck" = spellcheck, "src" = src, "style" = style, "tabindex" = tabindex, "title" = title, "width" = width, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<video ", attrs, ">")
x <- paste0(s_tag, ..., "</video>")
}else{
x <- paste0("<video>", ..., "</video>")
}
return(x)
}

#' Generate HTML tag for element wbr
#'
#' @param ... A string or strings of permitted content for this HTML element tag. The user is responsible for determining what is permissible.
#' @param accesskey A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param autocapitalize A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param class A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param contenteditable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param dir A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param draggable A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param hidden A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param id A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param inputmode A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param is A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemid A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemprop A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemref A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemscope A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param itemtype A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param lang A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param part A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param slot A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param spellcheck A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param style A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param tabindex A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param title A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @param custom_attr A string of permitted content for this HTML attribute. The user is responsible for determining what is permissible.
#' @return A HTML tag string.
#' @examples
#' wbr(class = "test", "Example")
wbr <- function(
...,
accesskey = NULL,
autocapitalize = NULL,
class = NULL,
contenteditable = NULL,
dir = NULL,
draggable = NULL,
hidden = NULL,
id = NULL,
inputmode = NULL,
is = NULL,
itemid = NULL,
itemprop = NULL,
itemref = NULL,
itemscope = NULL,
itemtype = NULL,
lang = NULL,
part = NULL,
slot = NULL,
spellcheck = NULL,
style = NULL,
tabindex = NULL,
title = NULL,
custom_attr = NULL){
attr_values <- c("accesskey" = accesskey, "autocapitalize" = autocapitalize, "class" = class, "contenteditable" = contenteditable, "dir" = dir, "draggable" = draggable, "hidden" = hidden, "id" = id, "inputmode" = inputmode, "is" = is, "itemid" = itemid, "itemprop" = itemprop, "itemref" = itemref, "itemscope" = itemscope, "itemtype" = itemtype, "lang" = lang, "part" = part, "slot" = slot, "spellcheck" = spellcheck, "style" = style, "tabindex" = tabindex, "title" = title, custom_attr)
attr_values <- attr_values[is.null(attr_values) == FALSE]
if(length(attr_values) > 0){
attr_names <- names(attr_values)
attr_values <- paste0('"', attr_values, '"')
attrs <- paste0(paste(attr_names, attr_values, sep = "="), collapse = " ")
s_tag <- paste0("<wbr ", attrs, ">")
x <- paste0(s_tag, ..., "</wbr>")
}else{
x <- paste0("<wbr>", ..., "</wbr>")
}
return(x)
}

