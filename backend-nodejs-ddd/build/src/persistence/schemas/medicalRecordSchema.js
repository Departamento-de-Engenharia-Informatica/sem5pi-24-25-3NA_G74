"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = __importDefault(require("mongoose"));
const MedicalRecordSchema = new mongoose_1.default.Schema({
    domainId: { type: String, unique: true },
    medicalRecords: { type: (Set) },
    allergies: { type: (Set) },
    description: { type: String },
}, {
    timestamps: true
});
//# sourceMappingURL=medicalRecordSchema.js.map