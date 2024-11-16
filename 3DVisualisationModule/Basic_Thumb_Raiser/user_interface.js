import * as THREE from "three";
import { GUI } from "three/addons/libs/lil-gui.module.min.js";
import {mazeParameters,buildingA1Data} from "./default_data.js";
import Maze from "./maze.js";
import { merge } from "./merge.js";

export default class UserInteraction {
    constructor(scene, renderer, lights, finalMaze, thumbRaser, animations) {

        this.buildingA1Parameters = merge({},buildingA1Data,mazeParameters);
        
        this.floorParametersA = new Array();
        this.floorParametersA.push(this.buildingA1Parameters);
        
        this.thumbRaser = thumbRaser;
        this.scene = scene;
        this.finalMaze = finalMaze;

        //thumbRaser.gameRunning=false;

        function colorCallback(object, color) {
            object.color.set(color);
        }

        function shadowsCallback(enabled) {
            scene.traverseVisible(function (child) { // Modifying the scene graph inside the callback is discouraged: https://threejs.org/docs/index.html?q=object3d#api/en/core/Object3D.traverseVisible
                if (child.material) {
                    child.material.needsUpdate = true;
                }
            });
        }

        function createEmoteCallback(animations, name) {
            callbacks[name] = function () {
                animations.fadeToAction(name, 0.2);
            };
            emotesFolder.add(callbacks, name);
        }

        // Create the graphical user interface
        this.gui = new GUI({ hideable: false });
        const buildingFolder = this.gui.addFolder('Buildings');

        
        //Create the building A folder
        const buildingA=buildingFolder.addFolder("Building A");
        
        // Add a button to change building parameters
        buildingA.add({ 'Floor 1': () => changeBuildingParameters(this.buildingA1Parameters) }, 'Floor 1');
        

        // Create the character folder
        const characterFolder = this.gui.addFolder("Character");

        // Create the emotes folder and add emotes
        const emotesFolder = characterFolder.addFolder("Emotes");
        const callbacks = [];
        for (let i = 0; i < animations.emotes.length; i++) {
            createEmoteCallback(animations, animations.emotes[i]);
        }

        function changeBuildingParameters(parameters){
            if (parameters) {
                console.log("New Floor:")
                console.log(parameters);
                // Cria um novo edifício com base nos parâmetros fornecidos
                createMaze(parameters);
            }
        }

        

        function createMaze(parameters) {
            // Remove o labirinto atual da cena se houver um
            if (finalMaze) {
                console.log("tr.doors");
                console.log(thumbRaser.maze.doors);
                for(let i=0; i< thumbRaser.maze.doors.length; i++){
                    scene.remove(thumbRaser.maze.doors[i].object);
                }

                scene.remove(thumbRaser.maze.object);

                thumbRaser.gameRunning=false;
                scene.remove(thumbRaser.gui);

            }

            // Cria um novo edifício
            finalMaze = new Maze(parameters,thumbRaser.doorParameters,0);
            finalMaze.scale= thumbRaser.maze.scale
            thumbRaser.maze= finalMaze;
        }


       /* function load(parameters){
            for (const [key, value] of Object.entries(parameters)) {
                this[key] = value;
            }


            // The cache must be enabled; additional information available at https://threejs.org/docs/api/en/loaders/FileLoader.html
            THREE.Cache.enabled = true;

            // Create a resource file loader
            const loader = new THREE.FileLoader();

            // Set the response type: the resource file will be parsed with JSON.parse()
            loader.setResponseType("json");

            // Load a maze description resource file
            loader.load(
                //Resource URL
                this.url,

                // onLoad callback
                description => this.onLoad(description),

                // onProgress callback
                xhr => this.onProgress(this.url, xhr),

                // onError callback
                error => this.onError(this.url, error)
            );
        }*/


        // Create the lights folder
        const lightsFolder = this.gui.addFolder("Lights");

        const directionalLightFolder = lightsFolder.addFolder("Directional light");
        const directionalLight = lights.object.directionalLight;
        const directionalColor = { color: "#" + new THREE.Color(directionalLight.color).getHexString() };
        directionalLightFolder.addColor(directionalColor, "color").onChange(color => colorCallback(directionalLight, color));
        directionalLightFolder.add(lights.object.directionalLight, "intensity", 0.0, 1.0, 0.01);

        // Create the ambient light folder
        const ambientLightFolder = lightsFolder.addFolder("Ambient light");
        const ambientLight = lights.object.ambientLight;
        const ambientColor = { color: "#" + new THREE.Color(ambientLight.color).getHexString() };
        ambientLightFolder.addColor(ambientColor, "color").onChange(color => colorCallback(ambientLight, color));
        ambientLightFolder.add(lights.object.ambientLight, "intensity", 0.0, 1.0, 0.01);

        // Create point light #1 folder
        const pointLight1Folder = lightsFolder.addFolder("Point light #1");
        const pointLight1 = lights.object.pointLight1;
        const pointColor1 = { color: "#" + new THREE.Color(pointLight1.color).getHexString() };
        pointLight1Folder.addColor(pointColor1, "color").onChange(color => colorCallback(pointLight1, color));
        pointLight1Folder.add(lights.object.pointLight1, "intensity", 0.0, 100.0, 1.0);
        pointLight1Folder.add(lights.object.pointLight1, "distance", 0.0, 20.0, 0.01);
        pointLight1Folder.add(lights.object.pointLight1.position, "x", -10.0, 10.0, 0.01);
        pointLight1Folder.add(lights.object.pointLight1.position, "y", 0.0, 20.0, 0.01);
        pointLight1Folder.add(lights.object.pointLight1.position, "z", -10.0, 10.0, 0.01);

        // Create point light #2 folder
        const pointLight2Folder = lightsFolder.addFolder("Point light #2");
        const pointLight2 = lights.object.pointLight2;
        const pointColor2 = { color: "#" + new THREE.Color(pointLight2.color).getHexString() };
        pointLight2Folder.addColor(pointColor2, "color").onChange(color => colorCallback(pointLight2, color));
        pointLight2Folder.add(lights.object.pointLight2, "intensity", 0.0, 100.0, 1.0);
        pointLight2Folder.add(lights.object.pointLight2, "distance", 0.0, 20.0, 0.01);
        pointLight2Folder.add(lights.object.pointLight2.position, "x", -10.0, 10.0, 0.01);
        pointLight2Folder.add(lights.object.pointLight2.position, "y", 0.0, 20.0, 0.01);
        pointLight2Folder.add(lights.object.pointLight2.position, "z", -10.0, 10.0, 0.01);

        // Create the shadows folder
        const shadowsFolder = this.gui.addFolder("Shadows");
        shadowsFolder.add(renderer.shadowMap, "enabled").onChange(enabled => shadowsCallback(enabled));
    }

    createMaze(parameters, playerPosition) {
        // Remove o labirinto atual da cena se houver um
        if (this.finalMaze) {
            for(let i=0; i< this.thumbRaser.maze.doors.length; i++){
                this.scene.remove(this.thumbRaser.maze.doors[i].object);
            }

            this.scene.remove(this.thumbRaser.maze.object);

            this.thumbRaser.gameRunning=false;
            this.scene.remove(this.thumbRaser.gui);
        }

        // Cria um novo edifício
        this.finalMaze = new Maze(parameters, this.thumbRaser.doorParameters, playerPosition);
        this.finalMaze.scale = this.thumbRaser.maze.scale;
        this.thumbRaser.maze = this.finalMaze;

    }

    setVisibility(visible) {
        if (visible) {
            this.gui.show();
        }
        else {
            this.gui.hide();
        }
    }

}

