"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
var __param = (this && this.__param) || function (paramIndex, decorator) {
    return function (target, key) { decorator(target, key, paramIndex); }
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const BaseController_1 = require("../core/infra/BaseController");
const typedi_1 = require("typedi");
const config_1 = __importDefault(require("../../config"));
let MedicalRecordController = class MedicalRecordController extends BaseController_1.BaseController {
    constructor(medicalRecordServiceInstance) {
        super();
        this.medicalRecordServiceInstance = medicalRecordServiceInstance;
    }
    executeImpl() {
        throw new Error('Method not implemented.');
    }
    findByAllergy(allergy) {
        try {
            const medicalRecord = this.medicalRecordServiceInstance.findByAllergy(allergy);
            if (medicalRecord) {
                return medicalRecord;
            }
            else {
                return null;
            }
        }
        catch (e) {
            throw new Error("Stopped on Controller");
        }
    }
    findByMedicalCondition(medicalCondition) {
        try {
            const medicalRecords = this.medicalRecordServiceInstance.findByMedicalCondition(medicalCondition);
            if (medicalRecords) {
                return medicalRecords;
            }
            else {
                return null;
            }
        }
        catch (e) {
            throw new Error('Stopped in Controller.');
        }
    }
    updateByPatientId(patientId, updateData) {
        try {
            const medicalRecord = this.medicalRecordServiceInstance.updateByPatientId(patientId, updateData);
            if (medicalRecord) {
                return medicalRecord;
            }
            else {
                return null;
            }
        }
        catch (e) {
        }
        throw new Error('Method not implemented.');
    }
};
MedicalRecordController = __decorate([
    (0, typedi_1.Service)(),
    __param(0, (0, typedi_1.Inject)(config_1.default.services.medicalRecord.name)),
    __metadata("design:paramtypes", [Object])
], MedicalRecordController);
exports.default = MedicalRecordController;
//# sourceMappingURL=medicalRecordController.js.map