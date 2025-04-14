// This pipeline is set up for dev deploys from dev branch. Examples are 
// include for staging and master.

def agentLabel

if ( env.BRANCH_NAME == "main" ) {
     agentLabel = "prod"
} else if ( env.BRANCH_NAME == "staging" ) {
     agentLabel = "staging"
} else if ( env.BRANCH_NAME == "dev" ) {
     agentLabel = "dev" 
} else {
     // if it is not dev, staging, master treat it as a hotfix or 
     // feature branch 
     agentLabel = "test"
}

pipeline {
    agent {
        label agentLabel
    }
    triggers {
        pollSCM 'H/30 * * * *'
    }
    options {
        buildDiscarder(logRotator(numToKeepStr: '5'));
        timestamps();
        disableConcurrentBuilds();
        office365ConnectorWebhooks([
           [name: "ccd-viztool", url: "https://usepa.webhook.office.com/webhookb2/52666a1b-6ebc-415a-8066-05f621f63b23@88b378b3-6748-4867-acf9-76aacbeca6a7/IncomingWebhook/ae355b01a615454d9a2fe71a7e010544/48636d5a-2386-4753-ba37-3e8816a8313c", startNotification: true, notifyBackToNormal: true, notifyFailure: true, notifyRepeatedFailure: true, notifySuccess: true, notifyAborted: true]
        ])
    }

    stages {

        stage('Source Code') {
           when { expression 
              { env.BRANCH_NAME ==~ /(dev|staging|main)/ }
                  }
           steps {
               sh '''cd /data/code/viztool
                     id
                     git pull'''
           }
        }
        
        stage('Build') {
           when { expression 
              { env.BRANCH_NAME ==~ /(dev|staging|main)/ }
                  }
           steps {
               sh '''cd /data/code/viztool
                     docker build --tag viztool:$BUILD_NUMBER .
                     '''
           }
        }
        
        stage('Clean') {
           when { expression 
              { env.BRANCH_NAME ==~ /(dev|staging|main)/ }
                  }
           steps {
               sh '''cd /data/code/viztool
                     docker stop viztool
                     docker rm viztool
                     '''
           }
        }
        
        stage('Deploy') {
           when { expression 
              { env.BRANCH_NAME ==~ /(dev|staging|main)/ }
                  }
           steps {
               sh '''cd /data/code/viztool
                     docker run \
                            --name viztool \
                            -itd \
                            -p 8022:8000 \
                            --restart unless-stopped \
                            viztool:$BUILD_NUMBER
                     '''
           }
        }
        
    }
}
