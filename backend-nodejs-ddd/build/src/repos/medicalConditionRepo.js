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
Object.defineProperty(exports, "__esModule", { value: true });
const typedi_1 = require("typedi");
const mongoose_1 = require("mongoose");
const medicalConditionId_1 = require("../domain/medicalConditionId");
const MedicalConditionMap_1 = require("../mappers/MedicalConditionMap");
let MedicalConditionRepo = class MedicalConditionRepo {
    constructor(medicalConditionSchema, logger) {
        this.medicalConditionSchema = medicalConditionSchema;
        this.logger = logger;
    }
    async exists(medicalConditionId) {
        const idX = medicalConditionId instanceof medicalConditionId_1.MedicalConditionId ? medicalConditionId.id.toValue() : medicalConditionId;
        const query = { domainId: idX };
        const medicalConditionDocument = await this.medicalConditionSchema.findOne(query);
        return !!medicalConditionDocument;
    }
    async save(medicalCondition) {
        const query = { domainId: medicalCondition.id.toValue() };
        const medicalConditionDocument = await this.medicalConditionSchema.findOne(query);
        try {
            if (medicalConditionDocument === null) {
                const rawMedicalCondition = MedicalConditionMap_1.MedicalConditionMap.toPersistence(medicalCondition);
                const medicalConditionCreated = await this.medicalConditionSchema.create(rawMedicalCondition);
                return MedicalConditionMap_1.MedicalConditionMap.toDomain(medicalConditionCreated);
            }
            else {
                medicalConditionDocument.designation = medicalCondition.designation;
                medicalConditionDocument.description = medicalCondition.description;
                await medicalConditionDocument.save();
                return medicalCondition;
            }
        }
        catch (err) {
            throw err;
        }
    }
    async update(medicalCondition) {
        const query = { domainId: medicalCondition.id.toString() };
        const medicalConditionDocument = await this.medicalConditionSchema.findOne(query);
        if (medicalConditionDocument === null) {
            throw Error("medical condition not found, couldn't update");
        }
        else {
            medicalConditionDocument.description = medicalCondition.description;
            await medicalConditionDocument.save();
            return medicalCondition;
        }
    }
    async findByDescription(description) {
        const query = { description: description.toString() };
        const medicalConditionRecord = await this.medicalConditionSchema.findOne(query);
        if (medicalConditionRecord != null) {
            return MedicalConditionMap_1.MedicalConditionMap.toDomain(medicalConditionRecord);
        }
        else {
            throw Error("medical condition not found");
        }
    }
    async findByDesignation(designation) {
        const query = { designation: designation.toString() };
        const medicalConditionRecords = await this.medicalConditionSchema.find(query);
        if (medicalConditionRecords != null) {
            return medicalConditionRecords.map((medicalConditionRecord) => MedicalConditionMap_1.MedicalConditionMap.toDomain(medicalConditionRecord));
        }
        else {
            throw Error("medical condition not found");
        }
    }
    async findAll() {
        const medicalConditionRecord = await this.medicalConditionSchema.find();
        if (medicalConditionRecord != null) {
            return medicalConditionRecord.map((medicalConditionRecord) => MedicalConditionMap_1.MedicalConditionMap.toDomain(medicalConditionRecord));
        }
        else {
            throw Error("medical condition not found");
        }
    }
    async findById(medicalConditionId) {
        const idX = medicalConditionId instanceof medicalConditionId_1.MedicalConditionId ? medicalConditionId.id.toValue() : medicalConditionId;
        const query = { domainId: idX };
        const medicalConditionDocument = await this.medicalConditionSchema.findOne(query);
        if (medicalConditionDocument != null) {
            return MedicalConditionMap_1.MedicalConditionMap.toDomain(medicalConditionDocument);
        }
        else {
            throw Error("medical condition not found");
        }
    }
    async findByMedicalConditionCode(medicalConditionCode) {
        const query = { medicalConditionCode: medicalConditionCode.toString() };
        const medicalConditionRecord = await this.medicalConditionSchema.findOne(query);
        if (medicalConditionRecord != null) {
            return MedicalConditionMap_1.MedicalConditionMap.toDomain(medicalConditionRecord);
        }
        else {
            throw Error("medical condition not found");
        }
    }
};
MedicalConditionRepo = __decorate([
    (0, typedi_1.Service)(),
    __param(0, (0, typedi_1.Inject)('medicalConditionSchema')),
    __param(1, (0, typedi_1.Inject)('logger')),
    __metadata("design:paramtypes", [mongoose_1.Model, Object])
], MedicalConditionRepo);
exports.default = MedicalConditionRepo;
//# sourceMappingURL=medicalConditionRepo.js.map