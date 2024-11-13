import * as THREE from "three";

export default class Animations_door {
    constructor(object, animations_door, location) {
        this.states = ["door|Open", "door|Close"];
        this.mixer = new THREE.AnimationMixer(object);
        this.actions = {};

        this.location = location;

        for (let i = 0; i < animations_door.length; i++) {
            const clip = animations_door[i];
            const action = this.mixer.clipAction(clip);
            this.actions[clip.name] = action;

            // Configurar clipes de animação de porta
            action.clampWhenFinished = true;
            action.loop = THREE.LoopOnce;
        }

        this.activeName = "door|Close";
        this.actions[this.activeName].play();
        
    }

    fadeToAction(name, duration) {
        if (this.activeName !== name && this.states.includes(name)) {
            const previousName = this.activeName;
            this.activeName = name;

            // Desvanecer e reproduzir a nova ação
            this.actions[previousName].fadeOut(duration);
            this.actions[this.activeName]
                .reset()
                .setEffectiveTimeScale(1)
                .setEffectiveWeight(1)
                .fadeIn(duration)
                .play();

        }
    }

    update(deltaT) {
        if (this.mixer) {
            this.mixer.update(deltaT);
        }
    }
}