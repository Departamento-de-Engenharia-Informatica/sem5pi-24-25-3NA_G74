"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = __importDefault(require("mongoose"));
const MedicalConditionSchema = new mongoose_1.default.Schema({
    domainId: { type: String, unique: true },
    medicalConditionCode: { type: String, unique: true },
    designation: { type: String },
    description: { type: String },
    commonSymptoms: { type: String }
}, {
    timestamps: true
});
exports.default = mongoose_1.default.model('MedicalCondition', MedicalConditionSchema);
//# sourceMappingURL=medicalConditionSchema.js.map