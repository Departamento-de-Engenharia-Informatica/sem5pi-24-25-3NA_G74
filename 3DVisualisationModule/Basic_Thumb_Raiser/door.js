import * as THREE from "three";
import { GLTFLoader } from "three/addons/loaders/GLTFLoader.js";

export default class Door {
    constructor(parameters, location) {
        this.onLoad = function (description) {
            this.object = description.scene;
            this.animations_door = description.animations;


            // Turn on shadows for this object
            this.setShadow(this.object);

            // Get the object's axis-aligned bounding box (AABB) in 3D space
            const box = new THREE.Box3();
            box.setFromObject(this.object);

            // Compute the object size
            const size = new THREE.Vector3();
            box.getSize(size);

            // Adjust the object's dimensions
            size.x = 3.0;
            size.y = 4.4;
            size.z = 2.6;

            // Set the object's scale
            this.object.scale.set(this.scale.x, this.scale.y, this.scale.z);
            this.loaded = true;
        }

        this.onProgress = function (url, xhr) {
            console.log("Resource '" + url + "' " + (100.0 * xhr.loaded / xhr.total).toFixed(0) + "% loaded.");
        }

        this.onError = function (url, error) {
            console.error("Error loading resource " + url + " (" + error + ").");
        }

        for (const [key, value] of Object.entries(parameters)) {
            this[key] = value;
        }

        this.openingSpeed = THREE.MathUtils.degToRad(this.openingSpeed);
        this.loaded = false;

        // Create a resource .gltf or .glb file loader
        const loader = new GLTFLoader();

        // Load a model description resource file
        loader.load(
            // Resource URL
            this.url,

            // onLoad callback
            description => this.onLoad(description),

            // onProgress callback
            xhr => this.onProgress(this.url, xhr),

            // onError callback
            error => this.onError(this.url, error)
        );

        this.location = location;

    }

    setShadow(object) {
        object.traverseVisible(function (child) {
            if (child instanceof THREE.Object3D) {
                child.castShadow = true;
                child.receiveShadow = false;
            }
        });
    }
}
