"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = __importDefault(require("mongoose"));
const MedicalRecordSchema = new mongoose_1.default.Schema({
    patientId: {
        type: String,
        required: true,
        unique: true,
        index: true, // Optimize queries on this field
    },
    allergies: [
        {
            type: String,
            ref: 'Allergy',
        },
    ],
    medicalConditions: [
        {
            type: String,
            ref: 'MedicalCondition',
        },
    ],
    freeText: String,
}, { timestamps: true });
exports.default = mongoose_1.default.model('MedicalRecord', MedicalRecordSchema);
//# sourceMappingURL=medicalRecordSchema.js.map