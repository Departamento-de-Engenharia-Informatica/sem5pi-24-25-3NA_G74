import { Allergy } from '../domain/Allergy';
import { MedicalCondition } from '../domain/medicalCondition';

export default interface IMedicalRecordDTO {
  id: string; // The domainId
  medicalRecordCode: string;
  allergies: string[]; // Array of allergy ObjectIds
  medicalConditions: string[];
  freeText: string;
  createdAt: string;
  updatedAt: string;
}
