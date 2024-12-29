"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const dotenv_1 = __importDefault(require("dotenv"));
const path_1 = __importDefault(require("path"));
// Set the NODE_ENV to 'development' by default
process.env.NODE_ENV = process.env.NODE_ENV || 'development';
const envFound = dotenv_1.default.config();
if (!envFound) {
    // This error should crash whole process
    throw new Error("⚠️  Couldn't find .env file  ⚠️");
}
exports.default = {
    /**
     * Your favorite port : optional change to 4000 by JRT
     */
    port: parseInt(process.env.PORT, 10) || 4000,
    /**
     * That long string from mlab
     */
    databaseURL: process.env.MONGODB_URI || 'mongodb://127.0.0.1:27017/test',
    /**
     * Your secret sauce
     */
    jwtSecret: process.env.JWT_SECRET || 'gRffUX0awziyoHA12LnqbheRYYlMttxg',
    /**
     * Used by winston logger
     */
    logs: {
        level: process.env.LOG_LEVEL || 'info',
    },
    /**
     * API configs
     */
    api: {
        prefix: '/api',
    },
    controllers: {
        role: {
            name: 'RoleController',
            path: '../controllers/roleController',
        },
        medicalCondition: {
            name: 'MedicalConditionController',
            path: '../controllers/medicalConditionController',
        },
        allergy: {
            name: 'AllergyController',
            path: '../controllers/allergyController',
        },
        medicalRecord: {
            name: 'MedicalRecordController',
            path: '../controllers/medicalRecordController',
        },
    },
    repos: {
        role: {
            name: 'RoleRepo',
            path: '../repos/roleRepo',
        },
        user: {
            name: 'UserRepo',
            path: '../repos/userRepo',
        },
        medicalCondition: {
            name: 'MedicalConditionRepo',
            path: '../repos/medicalConditionRepo',
        },
        allergy: {
            name: 'AllergyRepo',
            path: '../repos/allergyRepo',
        },
        medicalRecord: {
            name: 'MedicalRecordRepo',
            path: '../repos/medicalRecordRepo',
        },
    },
    services: {
        role: {
            name: 'RoleService',
            path: '../services/roleService',
        },
        medicalCondition: {
            name: 'MedicalConditionService',
            path: '../services/medicalConditionService',
        },
        allergy: {
            name: 'AllergyService',
            path: '../services/allergyService',
        },
        medicalRecord: {
            name: 'MedicalRecordService',
            path: '../services/medicalRecordService',
        },
    },
};
//# sourceMappingURL=config.js.map