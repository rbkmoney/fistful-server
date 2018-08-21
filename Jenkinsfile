#!groovy
// -*- mode: groovy -*-

def finalHook = {
  runStage('store CT logs') {
    archive '_build/test/logs/'
  }
}

build('fistful-server', 'docker-host', finalHook) {
  checkoutRepo()
  loadBuildUtils('build-utils')

  def pipeDefault
  def withWsCache
  runStage('load pipeline') {
    env.JENKINS_LIB = "build-utils/jenkins_lib"
    pipeDefault = load("${env.JENKINS_LIB}/pipeDefault.groovy")
    withWsCache = load("${env.JENKINS_LIB}/withWsCache.groovy")
  }

  pipeDefault() {

    if (!masterlikeBranch()) {

      runStage('compile') {
        withGithubPrivkey {
          sh 'make wc_compile'
        }
      }

      /*
       * TODO: uncomment when linter warnings are fixed
       *
      runStage('lint') {
        sh 'make wc_lint'
      }
      */

      runStage('xref') {
        sh 'make wc_xref'
      }

      runStage('dialyze') {
        withWsCache("_build/default/rebar3_19.3_plt") {
          sh 'make wc_dialyze'
        }
      }

      runStage('test') {
        sh "make wdeps_test"
      }

    }

    runStage('make release') {
      withGithubPrivkey {
        sh "make wc_release"
      }
    }

    runStage('build image') {
      sh "make build_image"
    }

    try {
      if (masterlikeBranch()) {
        runStage('push image') {
          sh "make push_image"
        }
      }
    } finally {
      runStage('rm local image') {
        sh 'make rm_local_image'
      }
    }

  }
}
