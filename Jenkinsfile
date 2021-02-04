#!groovy
// -*- mode: groovy -*-

def finalHook = {
  runStage('store CT logs') {
    archive '_build/test/logs/'
  }
  runStage('store services logs') {
    archive 'test/log/'
  }
}

build('fistful-server', 'docker-host', finalHook) {
  checkoutRepo()
  loadBuildUtils('build-utils')

  def pipeErlangService
  runStage('load pipeline') {
    env.JENKINS_LIB = "build-utils/jenkins_lib"
    env.SH_TOOLS = "build-utils/sh"
    pipeErlangService = load("${env.JENKINS_LIB}/pipeErlangService.groovy")
  }

  pipeErlangService.runPipe(true, true)
}
