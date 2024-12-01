import { IMedicalConditionPersistence } from '../../dataschema/IMedicalConditionPersistence';
import mongoose from 'mongoose';

const MedicalConditionSchema = new mongoose.Schema(
  {
    domainId: { type: String, unique: true },
    description: { type: String, unique: true }
  },
  {
    timestamps: true
  }
);

export default mongoose.model<IMedicalConditionPersistence & mongoose.Document>('MedicalCondition', MedicalConditionSchema);
