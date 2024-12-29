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
const medicalRecordSchema_1 = __importDefault(require("../persistence/schemas/medicalRecordSchema"));
let MedicalRecordRepo = class MedicalRecordRepo {
    constructor(medicalRecordSchema = medicalRecordSchema_1.default) {
        this.medicalRecordSchema = medicalRecordSchema;
    }
    async findAll() {
        try {
            // Using populate to get the full allergy and condition objects instead of just IDs
            const records = await this.medicalRecordSchema.find({});
            // .populate('allergies')
            // .populate('medicalConditions');
            return records;
        }
        catch (err) {
            throw err;
        }
    }
    async create(recordData) {
        try {
            // First check if a record with this patientId already exists
            const existingRecord = await this.medicalRecordSchema.findOne({ patientId: recordData.patientId });
            if (existingRecord) {
                throw new Error('Medical record already exists for this patient');
            }
            const record = new this.medicalRecordSchema(recordData);
            const savedRecord = await record.save();
            return savedRecord;
        }
        catch (err) {
            if (err.code === 11000) {
                throw new Error('Medical record already exists for this patient');
            }
            throw err;
        }
    }
    async findById(id) {
        try {
            const record = await this.medicalRecordSchema.findById(id);
            return record;
        }
        catch (err) {
            throw err;
        }
    }
    async findByPatientId(patientId) {
        try {
            const record = await this.medicalRecordSchema.findOne({ patientId });
            return record;
        }
        catch (err) {
            throw err;
        }
    }
    async updateByPatientId(patientId, updateData) {
        try {
            const updatedRecord = await this.medicalRecordSchema.findOneAndUpdate({ patientId }, { $set: updateData }, { new: true, runValidators: true });
            if (!updatedRecord) {
                throw new Error('Medical record not found for this patient');
            }
            return updatedRecord;
        }
        catch (err) {
            throw err;
        }
    }
};
MedicalRecordRepo = __decorate([
    (0, typedi_1.Service)(),
    __param(0, (0, typedi_1.Inject)('medicalRecordSchema')),
    __metadata("design:paramtypes", [Object])
], MedicalRecordRepo);
exports.default = MedicalRecordRepo;
//# sourceMappingURL=medicalRecordRepo.js.map