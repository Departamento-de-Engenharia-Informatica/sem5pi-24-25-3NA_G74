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
const typedi_1 = require("typedi");
const medicalRecordRepo_1 = __importDefault(require("../repos/medicalRecordRepo"));
let MedicalRecordService = class MedicalRecordService {
    constructor(medicalRecordRepo) {
        this.medicalRecordRepo = medicalRecordRepo;
    }
    async getAll() {
        try {
            const records = await this.medicalRecordRepo.findAll();
            return { records };
        }
        catch (e) {
            throw e;
        }
    }
    async getByPatientId(patientId) {
        try {
            const record = await this.medicalRecordRepo.findByPatientId(patientId);
            if (!record) {
                throw new Error('Medical record not found for this patient');
            }
            return { record };
        }
        catch (e) {
            throw e;
        }
    }
    async create(recordData) {
        try {
            const record = await this.medicalRecordRepo.create(recordData);
            return record;
        }
        catch (e) {
            throw e;
        }
    }
    async updateByPatientId(patientId, updateData) {
        try {
            // Validate that record exists before update
            const existingRecord = await this.medicalRecordRepo.findByPatientId(patientId);
            if (!existingRecord) {
                throw new Error('Medical record not found for this patient');
            }
            const record = await this.medicalRecordRepo.updateByPatientId(patientId, updateData);
            return record;
        }
        catch (e) {
            throw e;
        }
    }
    async findByMedicalCondition(medicalCondition) {
        try {
            console.log('Fetching all records');
            const records = await this.medicalRecordRepo.findAll();
            const filteredRecords = records.filter(record => record.medicalConditions.includes(medicalCondition));
            if (filteredRecords.length === 0) {
                throw new Error("Medical Condition doesn't exist.");
            }
            console.log('Filtered records:', filteredRecords);
            return filteredRecords;
        }
        catch (e) {
            console.error('Error in findByMedicalCondition:', e);
            throw e;
        }
    }
    async findByAllergy(allergy) {
        try {
            console.log('Fetching all records');
            const records = await this.medicalRecordRepo.findAll();
            const filteredRecords = records.filter(record => record.allergies.includes(allergy));
            if (filteredRecords.length === 0) {
                throw new Error("Allergy doesn't exist.");
            }
            console.log('Filtered records:', filteredRecords);
            return filteredRecords;
        }
        catch (e) {
            console.error('Error in findByMedicalCondition:', e);
            throw e;
        }
    }
};
MedicalRecordService = __decorate([
    (0, typedi_1.Service)(),
    __param(0, (0, typedi_1.Inject)('MedicalRecordRepo')),
    __metadata("design:paramtypes", [medicalRecordRepo_1.default])
], MedicalRecordService);
exports.default = MedicalRecordService;
//# sourceMappingURL=medicalRecordService.js.map