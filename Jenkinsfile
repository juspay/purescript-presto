def isVersionBumpCandidateBranch(branch) {
  (branch =~ /(main|hotfix-)/)
}

pipeline {
  agent {
    label "sdk"
  }
  environment {
      NPM_REPO_NAME = "purescript-presto"
      GIT_AUTHOR_NAME = 'jenkins.user'
      GIT_AUTHOR_EMAIL = 'jenkins.user@juspay.in'
      GIT_COMMITTER_NAME = 'jenkins.user'
      GIT_COMMITTER_EMAIL = 'jenkins.user@juspay.in'
	GIT_USERNAME = 'jenkins.user'
	GIT_EMAIL = 'jenkins.user@juspay.in'

  }

  stages {
    stage('Checkout') {
        steps {
            scmSkip(deleteBuild: false, skipPattern:'.*\\[skip ci\\].*')
            script {
              if (sh(script: "git log -1 --pretty=%B | grep -F -ie '[skip ci]' -e '[ci skip]'", returnStatus: true) == 0) {
                currentBuild.result = 'ABORTED'
                error 'Aborting because commit message contains [skip ci]'
              }
            }
        }
    }

    stage("npm/spago install") {
      steps {
        script {
          sh ("npm config set package-lock=false; npm i")
          sh ("npm run spago:install")
        }
      }
    }

    stage("PS compilation") {
      steps {
        script {
          // build packages to ensure compilation / tests work fine before updating version
          sh ("npm run compile")
        }
      }
    }

    stage("npm release") {
      steps {
        script {
          if (isVersionBumpCandidateBranch(env.BRANCH_NAME)) {
            sh ("npx semantic-release --debug")
          }
        }
      }
    }
  }
}
