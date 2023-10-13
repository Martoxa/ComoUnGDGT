# maybe useful down the line to manipulate whole groups of GDGTs
GDGTs<-list(isoGDGTs=list("GDGT0","GDGT1","GDGT2","GDGT3","Crena","Crenp"),brGDGTs=list("IIIa5","IIIa6","IIIb5","IIIb6","IIIc5","IIIc6","IIa5","IIa6","IIb5","IIb6","IIc5","IIc6","Ia","Ib","Ic"))

correctGs<-function(eq){
  sub("fa.","",unlist(strsplit(eq," "))[grep("fa",unlist(strsplit(eq," ")))])
}

partialEq<-function(fa,eq){
  out<-unlist(strsplit(eq," "))
  if(TRUE %in% is.na(match(out[grep("fa",out)],paste0("fa$",colnames(fa))))){
    warning(
      paste("Removed variables:",paste(
          sub("fa.","",out[grep("fa",out)[is.na(
                match(out[grep("fa",out)],paste0("fa$",colnames(fa)))
                )]]),collapse=","))
      )
  }
  out[grep("fa",out)[is.na(match(out[grep("fa",out)],paste0("fa$",colnames(fa))))]]<-0
  out<-paste(out,collapse=" ")
  out
}