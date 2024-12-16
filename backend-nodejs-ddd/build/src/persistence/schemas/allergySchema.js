"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const mongoose_1 = __importDefault(require("mongoose"));
const Allergy = new mongoose_1.default.Schema({
    domainId: {
        type: String,
        unique: true
    },
    code: {
        type: String,
        unique: true,
        required: [true, 'Please enter code']
    },
    designation: {
        type: String,
        required: [true, 'Please enter designation']
    },
    description: {
        type: String,
        required: [true, 'Please enter description']
    },
}, { timestamps: true });
exports.default = mongoose_1.default.model('Allergy', Allergy);
//# sourceMappingURL=allergySchema.js.map