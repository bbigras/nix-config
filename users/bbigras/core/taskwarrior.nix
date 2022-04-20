{
  programs.taskwarrior = {
    enable = true;
    colorTheme = "dark-blue-256";
    config = {
      confirmation = false;
      report.minimal.filter = "status:pending";
      report.active.columns = [ "id" "start" "entry.age" "priority" "project" "due" "description" ];
      report.active.labels = [ "ID" "Started" "Age" "Priority" "Project" "Due" "Description" ];
    };
    extraConfig = ''
      # Bugwarrior UDAs
      uda.githubtitle.type=string
      uda.githubtitle.label=Github Title
      uda.githubbody.type=string
      uda.githubbody.label=Github Body
      uda.githubcreatedon.type=date
      uda.githubcreatedon.label=Github Created
      uda.githubupdatedat.type=date
      uda.githubupdatedat.label=Github Updated
      uda.githubclosedon.type=date
      uda.githubclosedon.label=GitHub Closed
      uda.githubmilestone.type=string
      uda.githubmilestone.label=Github Milestone
      uda.githubrepo.type=string
      uda.githubrepo.label=Github Repo Slug
      uda.githuburl.type=string
      uda.githuburl.label=Github URL
      uda.githubtype.type=string
      uda.githubtype.label=Github Type
      uda.githubnumber.type=numeric
      uda.githubnumber.label=Github Issue/PR #
      uda.githubuser.type=string
      uda.githubuser.label=Github User
      uda.githubnamespace.type=string
      uda.githubnamespace.label=Github Namespace
      uda.githubstate.type=string
      uda.githubstate.label=GitHub State
      uda.gmailthreadid.type=string
      uda.gmailthreadid.label=GMail Thread Id
      uda.gmailsubject.type=string
      uda.gmailsubject.label=GMail Subject
      uda.gmailurl.type=string
      uda.gmailurl.label=GMail URL
      uda.gmaillastsender.type=string
      uda.gmaillastsender.label=GMail last sender name
      uda.gmaillastsenderaddr.type=string
      uda.gmaillastsenderaddr.label=GMail last sender address
      uda.gmaillastmessageid.type=string
      uda.gmaillastmessageid.label=Last RFC2822 Message-ID
      uda.gmailsnippet.type=string
      uda.gmailsnippet.label=GMail snippet
      uda.gmaillabels.type=string
      uda.gmaillabels.label=GMail labels
      uda.trellocard.type=string
      uda.trellocard.label=Trello card name
      uda.trellocardid.type=string
      uda.trellocardid.label=Trello card ID
      uda.trellocardidshort.type=numeric
      uda.trellocardidshort.label=Trello short card ID
      uda.trellodescription.type=string
      uda.trellodescription.label=Trello description
      uda.trelloboard.type=string
      uda.trelloboard.label=Trello board name
      uda.trellolist.type=string
      uda.trellolist.label=Trello list name
      uda.trelloshortlink.type=string
      uda.trelloshortlink.label=Trello shortlink
      uda.trelloshorturl.type=string
      uda.trelloshorturl.label=Trello short URL
      uda.trellourl.type=string
      uda.trellourl.label=Trello URL
      # END Bugwarrior UDAs

      # https://old.reddit.com/r/taskwarrior/comments/ta4hah/handling_delegation/hzyq9om/
      uda.delegated.type=string
      uda.delegated.label=Delegated to
      report.ready.filter=+READY delegated:
      report.delegated.description=Tasks delegated to others
      report.delegated.columns=id,project,description,delegated
      report.delegated.sort=delegated+,project+,description+
      report.delegated.filter=delegated.not: +PENDING

      context.work.read=+work
      context.work.write=+work
      context.home.read=-work -freelance -lab
      context.home.write=

      news.version=2.6.0
      reserved.lines=3
      report.ready.reserved.lines=3

      report.top.columns=id,priority,project,tags,description.count
      report.top.description='Minimal details of tasks'
      report.top.filter=status:pending (priority:H or priority:M)
      report.top.labels=ID,Pri,Project,Tags,Description
      report.top.sort=priority-/,project-,description+
    '';
  };
}
