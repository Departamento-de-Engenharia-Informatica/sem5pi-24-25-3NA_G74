import { IMedicalConditionPersistence } from '../../dataschema/IMedicalConditionPersistence';
import mongoose from 'mongoose';
import { MedicalRecord } from '../../domain/MedicalRecord';
import { Allergy } from '../../domain/Allergy';


const MedicalRecordSchema = new mongoose.Schema(
    {
      domainId: { type: String, unique: true },
      medicalRecords: { type: Set<MedicalRecord> },
      allergies: { type: Set<Allergy> },
      description: { type: String },
      
    },
    {
      timestamps: true
    }
  );