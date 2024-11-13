import * as THREE from "three";

export default class Animations {
    constructor(object, animations) {
        
        this.states = ["metarig|Walk", "metarig|Idle(HeavyBreathing)","metarig|Dance1"];
        this.emotes = ["metarig|Dance1"];  

        this.mixer = new THREE.AnimationMixer(object);
        this.actionInProgress = false;

        console.log("anime");
        console.log(animations);

        this.actions = {};
        for (let i = 0; i < animations.length; i++) {
            const clip = animations[i];
            const action = this.mixer.clipAction(clip);
            this.actions[clip.name] = action;
            if (this.emotes.indexOf(clip.name) == 0) {
                action.clampWhenFinished = true;
                action.loop = THREE.LoopOnce;
            }
        }
        //this.activeName = "IDLE";
        this.activeName = "metarig|Idle(HeavyBreathing)";
        this.actions[this.activeName].play();
    }

    fadeToAction(name, duration) {
        if (this.activeName != name && !this.actionInProgress) {
            const previousName = this.activeName;
            this.activeName = name;
            this.actions[previousName].fadeOut(duration);
            this.actions[this.activeName]
                .reset()
                .setEffectiveTimeScale(1)
                .setEffectiveWeight(1)
                .fadeIn(duration)
                .play();
            // Some actions must not be interrupted
            if (this.activeName != "metarig|Idle(HeavyBreathing)" && this.activeName != "metarig|Walk" ) {
                this.mixer.addEventListener("finished", event => this.actionFinished(event));
                this.actionInProgress = true;
            }

            if (this.activeName != "metarig|Idle(HeavyBreathing)" && this.activeName != "metarig|Walk") {
                this.mixer.addEventListener("finished", event => this.actionFinished(event));
                this.actionInProgress = true;
            }
        }
    }

    actionFinished() {
        if (this.actionInProgress) {
            this.actionInProgress = false;
            this.mixer.removeEventListener("finished", this.actionInProgress);
        }
    }

    update(deltaT) {
        if (this.mixer) {
            this.mixer.update(deltaT);
        }
    }
}
