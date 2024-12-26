export interface MedicalRecordDTO{
    patientId: string;
    medicalConditions: string[];
    allergies: string[];
    freeText: string;   
}