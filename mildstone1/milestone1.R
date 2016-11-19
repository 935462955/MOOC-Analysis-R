# task 1: engagement index

# read table
mooc = read.table("C:/Users/Yumeng/Dropbox/MOOCprj/user-activities.Rda")

# define variables with cumulative numbers of each behavior
mooc$CumulativeView = mooc$LectureView.1+mooc$LectureView.2+mooc$LectureView.3
mooc$CumulativeReView = mooc$LectureReView.1+mooc$LectureReView.2+mooc$LectureReView.3
mooc$CumulativeSubmission = mooc$AssignmentSubmission.1+mooc$AssignmentSubmission.2+mooc$AssignmentSubmission.3
mooc$CumulativeReSubmission = mooc$AssignmentReSubmission.1+mooc$AssignmentReSubmission.2+mooc$AssignmentReSubmission.3
mooc$CumulativeForumView = mooc$ForumView.1+mooc$ForumView.2+mooc$ForumView.3
mooc$CumulativeThreadLaunch = mooc$ThreadLaunch.1+mooc$ThreadLaunch.2+mooc$ThreadLaunch.3
mooc$CumulativePostOn = mooc$PostonThread.1+mooc$PostonThread.2+mooc$PostonThread.3

# define (passive/active) engagement index
mooc$passiveEngagement = mooc$CumulativeView+mooc$CumulativeReView+mooc$CumulativeForumView
mooc$activeEngagement = mooc$CumulativeSubmission+mooc$CumulativeReSubmission+mooc$CumulativeThreadLaunch+mooc$CumulativePostOn

# check correlation between passive & active engagements
t.test(mooc$passiveEngagement, mooc$activeEngagement)
plot(mooc$passiveEngagement~ mooc$activeEngagement)


########################################

# task 2
cpp= subset(mooc,course=='cpp')
java = subset(mooc,course=='java')

cpp$PassiveEngagement.1 = cpp$LectureReView.1 + cpp$LectureView.1 + cpp$ForumView.1
cpp$ActiveEngagement.1 = cpp$AssignmentSubmission.1+cpp$AssignmentReSubmission.1+cpp$ThreadLaunch.1+cpp$PostonThread.1
cpp$PassiveEngagement.2 = cpp$LectureReView.2 + cpp$LectureView.2 + cpp$ForumView.2
cpp$ActiveEngagement.2 = cpp$AssignmentSubmission.2+cpp$AssignmentReSubmission.2+cpp$ThreadLaunch.2+cpp$PostonThread.2
cpp$PassiveEngagement.3 = cpp$LectureReView.3 + cpp$LectureView.3 + cpp$ForumView.3
cpp$ActiveEngagement.3 = cpp$AssignmentSubmission.3+cpp$AssignmentReSubmission.3+cpp$ThreadLaunch.3+cpp$PostonThread.3

cpp$OverallEngagement.1 = cpp$PassiveEngagement.1+cpp$ActiveEngagement.1
cpp$OverallEngagement.2 = cpp$PassiveEngagement.2+cpp$ActiveEngagement.2
cpp$OverallEngagement.3 = cpp$PassiveEngagement.3+cpp$ActiveEngagement.3

java$PassiveEngagement.1 = java$LectureReView.1 + java$LectureView.1 + java$ForumView.1
java$ActiveEngagement.1 = java$AssignmentSubmission.1+java$AssignmentReSubmission.1+java$ThreadLaunch.1+java$PostonThread.1
java$PassiveEngagement.2 = java$LectureReView.2 + java$LectureView.2 + java$ForumView.2
java$ActiveEngagement.2 = java$AssignmentSubmission.2+java$AssignmentReSubmission.2+java$ThreadLaunch.2+java$PostonThread.2
java$PassiveEngagement.3 = java$LectureReView.3 + java$LectureView.3 + java$ForumView.3
java$ActiveEngagement.3 = java$AssignmentSubmission.3+java$AssignmentReSubmission.3+java$ThreadLaunch.3+java$PostonThread.3

java$OverallEngagement.1 = java$PassiveEngagement.1+java$ActiveEngagement.1
java$OverallEngagement.2 = java$PassiveEngagement.2+java$ActiveEngagement.2
java$OverallEngagement.3 = java$PassiveEngagement.3+java$ActiveEngagement.3

overallM = matrix(data = c(mean(cpp$OverallEngagement.1),mean(cpp$OverallEngagement.2),mean(cpp$OverallEngagement.3), mean(java$OverallEngagement.1),mean(java$OverallEngagement.2),mean(java$OverallEngagement.3)), nrow =3, ncol=2)
matplot(overallM, type = c("b"),pch=1,col = 1:2,main="Weekly Overall engagement")
legend("topright", legend = c("cpp","java"), col=1:2, pch=1)

passiveM = matrix(data = c(mean(cpp$PassiveEngagement.1),mean(cpp$PassiveEngagement.2),mean(cpp$PassiveEngagement.3), mean(java$PassiveEngagement.1),mean(java$PassiveEngagement.2),mean(java$PassiveEngagement.3)), nrow =3, ncol=2)
matplot(passiveM , type = c("b"),pch=1,col = 1:2,main="Weekly Passive engagement")
legend("topright", legend = c("cpp","java"), col=1:2, pch=1)


activeM = matrix(data = c(mean(cpp$ActiveEngagement.1),mean(cpp$ActiveEngagement.2),mean(cpp$ActiveEngagement.3), mean(java$ActiveEngagement.1),mean(java$ActiveEngagement.2),mean(java$ActiveEngagement.3)), nrow =3, ncol=2)
matplot(activeM , type = c("b"),pch=1,col = 1:2,main="Weekly Active engagement")
legend("topleft", legend = c("cpp","java"), col=1:2, pch=1)


boxplot(cpp$PassiveEngagement.1, java$PassiveEngagement.1,cpp$PassiveEngagement.2, java$PassiveEngagement.2,cpp$PassiveEngagement.3, java$PassiveEngagement.3,
  main="Weekly passive engagemernt",
  at=c(1,2,4,5,7,8),
  names=c("cpp1", "java1", "cpp2", "java1", "cpp3", "java3"),
  las=2,
  col=c("orange","red"),
  border="brown",
  horizontal=TRUE,
  notch=TRUE
)

boxplot(cpp$ActiveEngagement.1, java$ActiveEngagement.1,cpp$ActiveEngagement.2, java$ActiveEngagement.2,cpp$ActiveEngagement.3, java$ActiveEngagement.3,
  main="Weekly active engagemernt",
  at=c(1,2,4,5,7,8),
  names=c("cpp1", "java1", "cpp2", "java1", "cpp3", "java3"),
  las=2,
  col=c("orange","red"),
  border="brown",
  horizontal=TRUE,
  notch=TRUE
)

boxplot(cpp$OverallEngagement.1, java$OverallEngagement.1,cpp$OverallEngagement.2, java$OverallEngagement.2,cpp$OverallEngagement.3, java$OverallEngagement.3,
  main="Weekly overall engagemernt",
  at=c(1,2,4,5,7,8),
  names=c("cpp1", "java1", "cpp2", "java1", "cpp3", "java3"),
  las=2,
  col=c("orange","red"),
  border="brown",
  horizontal=TRUE,
  notch=TRUE
)

########################################

# task 3

t.test(mooc$CumulativePostOn, mooc$normal_grade)
t.test(mooc$CumulativeThreadLaunch, mooc$normal_grade)
t.test(mooc$CumulativeThreadLaunch~ mooc$normal_grade)

lm(mooc$normal_grade ~mooc$CumulativeForumView)
lm(mooc$normal_grade ~mooc$CumulativeThreadLaunch)
lm(mooc$normal_grade ~mooc$CumulativePostOn)

plot(mooc$CumulativeForumView, mooc$normal_grade)
abline(lm(mooc$normal_grade ~mooc$CumulativeForumView))
plot(mooc$CumulativeThreadLaunch, mooc$normal_grade)
abline(lm(mooc$normal_grade ~mooc$CumulativeThreadLaunch))
plot(mooc$CumulativePostOn, mooc$normal_grade)
abline(lm(mooc$normal_grade ~mooc$CumulativePostOn))

mooc$otherActivities = mooc$CumulativeView+mooc$CumulativeReView+mooc$CumulativeSubmission+mooc$CumulativeReSubmission
lm(mooc$normal_grade~ mooc$CumulativeForumView+mooc$CumulativeThreadLaunch+mooc$CumulativePostOn+mooc$otherActivities)

########################################

# task 6

lm(mooc$normal_grade~ mooc$CumulativeView+mooc$CumulativeReView+mooc$CumulativeSubmission+mooc$CumulativeReSubmission+mooc$CumulativeForumView+mooc$CumulativeThreadLaunch+mooc$CumulativePostOn)

lm(mooc$normal_grade~ mooc$passiveEngagement+mooc$activeEngagement)