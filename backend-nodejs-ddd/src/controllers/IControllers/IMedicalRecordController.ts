import { NextFunction, Request, Response } from "express";
import { MedicalRecord } from "../../domain/medicalRecord";


export default interface IMedicalRecordController{
    findByAllergy(allergy: string):Promise<MedicalRecord[]>;
    findByMedicalCondition(medicalCondition: string):Promise<MedicalRecord[]>;
    updateByPatientId(patientId: string, updateData: any): Promise<MedicalRecord>;
    
}