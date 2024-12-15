import { Allergy } from "../domain/Allergy";
import { MedicalCondition } from "../domain/medicalCondition";

export interface IMedicalRecordPersistence
{
    _id: string;
    medicalRecordCode: string;
    allergies: Set<Allergy>;
    medicalCondition: Set<MedicalCondition>;
    designation: string;
    
}