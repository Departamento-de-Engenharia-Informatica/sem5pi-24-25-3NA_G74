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
const config_1 = __importDefault(require("../../config"));
const MedicalConditionMap_1 = require("../mappers/MedicalConditionMap");
const medicalCondition_1 = require("../domain/medicalCondition");
const Result_1 = require("../core/logic/Result");
let MedicalConditionService = class MedicalConditionService {
    constructor(medicalConditionRepo) {
        this.medicalConditionRepo = medicalConditionRepo;
    }
    async createMedicalCondition(medicalConditionDTO) {
        try {
            const medicalConditionOrError = medicalCondition_1.MedicalCondition.create(medicalConditionDTO);
            if (medicalConditionOrError.isFailure) {
                return Result_1.Result.fail(medicalConditionOrError.errorValue());
            }
            const medicalConditionResult = medicalConditionOrError.getValue();
            await this.medicalConditionRepo.save(medicalConditionResult);
            const medicalConditionDTOResult = MedicalConditionMap_1.MedicalConditionMap.toDTO(medicalConditionResult);
            return Result_1.Result.ok(medicalConditionDTOResult);
        }
        catch (e) {
            throw e;
        }
    }
    async UpdateMedicalCondition(medicalConditionDTO) {
        try {
            const medicalCondition = await this.medicalConditionRepo.findByMedicalConditionCode(medicalConditionDTO.medicalConditionCode);
            if (medicalCondition === null) {
                return Result_1.Result.fail("MedicalCondition not found");
            }
            else {
                if (medicalConditionDTO.designation != null) {
                    medicalCondition.designation = medicalConditionDTO.designation;
                }
                if (medicalConditionDTO.description != null) {
                    medicalCondition.description = medicalConditionDTO.description;
                }
                await this.medicalConditionRepo.save(medicalCondition);
                const medicalConditionDTOResult = MedicalConditionMap_1.MedicalConditionMap.toDTO(medicalCondition);
                return Result_1.Result.ok(medicalConditionDTOResult);
            }
        }
        catch (e) {
            throw e;
        }
    }
    async SearchMedicalCondition(medicalConditionCode, designation) {
        try {
            if (medicalConditionCode != null) {
                const medicalCondition = await this.medicalConditionRepo.findByMedicalConditionCode(medicalConditionCode);
                const medicalConditionDTO = MedicalConditionMap_1.MedicalConditionMap.toDTO(medicalCondition);
                let medicalConditionDTOsArray = new Array();
                medicalConditionDTOsArray.push(medicalConditionDTO);
                return Result_1.Result.ok(medicalConditionDTOsArray);
            }
            if (designation != null) {
                const medicalConditions = await this.medicalConditionRepo.findByDesignation(designation);
                const medicalConditionDTOs = medicalConditions.map((medicalCondition) => MedicalConditionMap_1.MedicalConditionMap.toDTO(medicalCondition));
                return Result_1.Result.ok(medicalConditionDTOs);
            }
            else {
                const medicalConditions = await this.medicalConditionRepo.findAll();
                const medicalConditionDTOs = medicalConditions.map(medicalCondition => MedicalConditionMap_1.MedicalConditionMap.toDTO(medicalCondition));
                return Result_1.Result.ok(medicalConditionDTOs);
            }
        }
        catch (e) {
            throw e;
        }
    }
};
MedicalConditionService = __decorate([
    (0, typedi_1.Service)(),
    __param(0, (0, typedi_1.Inject)(config_1.default.repos.medicalCondition.name)),
    __metadata("design:paramtypes", [Object])
], MedicalConditionService);
exports.default = MedicalConditionService;
//# sourceMappingURL=medicalConditionService.js.map