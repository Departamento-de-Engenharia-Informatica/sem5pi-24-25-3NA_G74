import * as THREE from "three";

export default class ChandelierLight {
    constructor(parameters,x,y,z) {
        for (const [key, value] of Object.entries(parameters)) {
            this[key] = value;
        }

        // Create a group of objects
        this.object = new THREE.Group();

        // Create a PointLight for the chandelier
        this.object.chandelierLight = new THREE.PointLight(this.chandelierLight.color, this.chandelierLight.intensity, this.chandelierLight.distance);
        this.object.chandelierLight.position.set(x, y, z);

        // Set up shadow properties for this light
        this.object.chandelierLight.shadow.mapSize.width = 512;
        this.object.chandelierLight.shadow.mapSize.height = 512;
        this.object.chandelierLight.shadow.camera.near = 5.0;
        this.object.chandelierLight.shadow.camera.far = 15.0;
        this.object.add(this.object.chandelierLight);
    }

}
