import { IMedicalConditionPersistence } from '../../dataschema/IMedicalConditionPersistence';
import mongoose from 'mongoose';

const MedicalConditionSchema = new mongoose.Schema(
  {
    domainId: { type: String, unique: true },
    medicalConditionCode: { type: String, unique: true },
    designation: { type: String },
    description: { type: String },
    commonSymptoms: { type: String }
  },
  {
    timestamps: true
  }
);

export default mongoose.model<IMedicalConditionPersistence & mongoose.Document>('MedicalCondition', MedicalConditionSchema);
