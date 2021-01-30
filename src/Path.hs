module Path ( atgFolder, archiveFolder, currentLink, contestFile, modeFile
            , loginInfoFile, mainhsFile, mainbkhsFile, skeletonFile, testFolder
            , targetSrc, targetSrcBK, targetBin ) where

atgFolder = "./.atg"

archiveFolder = "./archive"

currentLink = "./current"

contestFile = atgFolder ++ "/contest.txt"

modeFile = atgFolder ++ "/mode.txt"

loginInfoFile = atgFolder ++ "/login.txt"

mainhsFile = "./app/Main.hs"

mainbkhsFile = atgFolder ++ "/Main.bk.hs"

skeletonFile = atgFolder ++ "/sleleton.hs"

testFolder = atgFolder ++ "/test"

targetSrc contestName taskName
  = archiveFolder ++ "/" ++ contestName ++ "/" ++ taskName ++ ".hs"

targetSrcBK contestName taskName
  = archiveFolder ++ "/" ++ contestName ++ "/" ++ taskName ++ ".bk.hs"

targetBin contestName taskName
  = archiveFolder ++ "/" ++ contestName ++ "/" ++ taskName ++ ".out"