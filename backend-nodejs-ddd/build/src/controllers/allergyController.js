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
let AllergyController = class AllergyController extends BaseController_1.BaseController {
    constructor(allergyServiceInstance) {
        super();
        this.allergyServiceInstance = allergyServiceInstance;
    }
    executeImpl() {
        throw new Error('Method not implemented.');
    }
    async createAllergy(req, res, next) {
        try {
            const allergyOrError = await this.allergyServiceInstance.CreateAllergy(req.body);
            if (allergyOrError.isFailure) {
                return res.status(402).send();
            }
            const allergyDTO = allergyOrError.getValue();
            return res.status(201).json(allergyDTO);
        }
        catch (e) {
            this.fail(e);
            return next(e);
        }
    }
    async updateAllergy(req, res, next) {
        try {
            const { code } = req.params;
            const updatedData = req.body;
            const allergyOrError = await this.allergyServiceInstance.UpdateAllergy(Object.assign({ code }, updatedData));
            if (allergyOrError.isFailure) {
                return this.notFound("Allergy not found");
            }
            const allergyDTO = allergyOrError.getValue();
            return this.ok(res, allergyDTO);
        }
        catch (e) {
            this.fail(e);
            return next(e);
        }
    }
    async searchAllergy(req, res, next) {
        try {
            const { code, designation } = req.query;
            const allergyOrError = await this.allergyServiceInstance.SearchAllergy(code, designation);
            if (allergyOrError.isFailure) {
                return res.status(404).send();
            }
            const allergyDTOs = allergyOrError.getValue();
            return this.ok(res, allergyDTOs);
        }
        catch (e) {
            this.fail(e);
            return next(e);
        }
    }
};
AllergyController = __decorate([
    (0, typedi_1.Service)(),
    __param(0, (0, typedi_1.Inject)(config_1.default.services.allergy.name)),
    __metadata("design:paramtypes", [Object])
], AllergyController);
exports.default = AllergyController;
//# sourceMappingURL=allergyController.js.map