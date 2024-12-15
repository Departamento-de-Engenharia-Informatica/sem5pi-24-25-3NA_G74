import { MedicalRecord } from "../domain/MedicalRecord";
import MedicalRecordId from "../domain/medicalRecordId";
import IMedicalRecordRepo from "../services/IRepos/IMedicalRecordRepo";



export default class MedicalRecordRepo implements IMedicalRecordRepo
{
    save(MedicalRecord: MedicalRecord): Promise<MedicalRecord> {
        throw new Error("Method not implemented.");
    }
    update(MedicalRecord: MedicalRecord): Promise<MedicalRecord> {
        throw new Error("Method not implemented.");
    }
    findByDescription(description: string): Promise<MedicalRecord> {
        throw new Error("Method not implemented.");
    }
    findByDesignation(designation: string): Promise<MedicalRecord[]> {
        throw new Error("Method not implemented.");
    }
    findAll(): Promise<MedicalRecord[]> {
        throw new Error("Method not implemented.");
    }
    findById(id: MedicalRecordId | string): Promise<MedicalRecord> {
        throw new Error("Method not implemented.");
    }
    findByMedicalConditionCode(medicalConditionCode: string): Promise<MedicalRecord> {
        throw new Error("Method not implemented.");
    }
    exists(t: MedicalRecord): Promise<boolean> {
        throw new Error("Method not implemented.");
    }

}