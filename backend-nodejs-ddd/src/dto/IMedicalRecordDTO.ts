import { Allergy } from "../domain/Allergy";
import { MedicalCondition } from "../domain/medicalCondition";

export default interface IMedicalRecordDTO
{
    id?: string;
    allergies: Set<Allergy>;
    medicalConditions: Set<MedicalCondition>;
    designation: string;
}