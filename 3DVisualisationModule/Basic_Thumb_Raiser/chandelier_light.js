import * as THREE from "three";

export default class ChandelierLight {
    constructor(parameters,x,y,z) {
        for (const [key, value] of Object.entries(parameters)) {
            this[key] = value;
        }

        // Create a group of objects
        this.object = new THREE.Group();

        // Create a PointLight for the chandelier
        this.object.chandelierLight = new THREE.SpotLight(this.chandelierLight.color, this.chandelierLight.intensity, this.chandelierLight.distance, Math.PI / 4, 0.5, 2);
        this.object.chandelierLight.position.set(x, y, z);
        this.object.chandelierLight.target.position.set(0, -5000, 0);
        this.object.add(this.object.chandelierLight.target);

        // Set up shadow properties for this light
        this.object.chandelierLight.shadow.mapSize.width = 512;
        this.object.chandelierLight.shadow.mapSize.height = 512;
        this.object.chandelierLight.shadow.camera.near = 5.0;
        this.object.chandelierLight.shadow.camera.far = 15.0;
        this.object.add(this.object.chandelierLight);
    }

}
