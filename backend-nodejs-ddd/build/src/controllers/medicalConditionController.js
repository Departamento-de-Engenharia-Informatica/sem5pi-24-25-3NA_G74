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
const BaseController_1 = require("../core/infra/BaseController");
let MedicalConditionController = class MedicalConditionController extends BaseController_1.BaseController {
    constructor(medicalConditionServiceInstance) {
        super();
        this.medicalConditionServiceInstance = medicalConditionServiceInstance;
    }
    executeImpl() {
        throw new Error('Method not implemented.');
    }
    async createMedicalCondition(req, res, next) {
        try {
            const medicalConditionOrError = await this.medicalConditionServiceInstance.createMedicalCondition(req.body);
            if (medicalConditionOrError.isFailure) {
                return res.status(402).send();
            }
            const medicalConditionDTO = medicalConditionOrError.getValue();
            return res.status(201).json(medicalConditionDTO);
        }
        catch (e) {
            this.fail(e);
            return next(e);
        }
    }
    async updateMedicalCondition(req, res, next) {
        try {
            const { medicalConditionCode } = req.params;
            const updatedData = req.body;
            const medicalConditionOrError = await this.medicalConditionServiceInstance.UpdateMedicalCondition(Object.assign({ medicalConditionCode }, updatedData));
            if (medicalConditionOrError.isFailure) {
                return this.notFound("Medical Condition not found");
            }
            const medicalConditionDTO = medicalConditionOrError.getValue();
            return this.ok(res, medicalConditionDTO);
        }
        catch (e) {
            this.fail(e);
            return next(e);
        }
    }
    async searchMedicalCondition(req, res, next) {
        try {
            const { medicalConditionCode, designation } = req.query;
            const medicalConditionOrError = await this.medicalConditionServiceInstance.SearchMedicalCondition(medicalConditionCode, designation);
            if (medicalConditionOrError.isFailure) {
                return res.status(404).send();
            }
            const medicalConditionDTOs = medicalConditionOrError.getValue();
            return this.ok(res, medicalConditionDTOs);
        }
        catch (e) {
            this.fail(e);
            return next(e);
        }
    }
};
MedicalConditionController = __decorate([
    (0, typedi_1.Service)(),
    __param(0, (0, typedi_1.Inject)(config_1.default.services.medicalCondition.name)),
    __metadata("design:paramtypes", [Object])
], MedicalConditionController);
exports.default = MedicalConditionController;
//# sourceMappingURL=medicalConditionController.js.map