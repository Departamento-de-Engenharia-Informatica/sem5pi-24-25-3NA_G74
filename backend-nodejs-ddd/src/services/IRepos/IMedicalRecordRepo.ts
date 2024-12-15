import { Repo } from "../../core/infra/Repo";
import { MedicalRecord } from "../../domain/MedicalRecord";
import MedicalRecordId from "../../domain/medicalRecordId";


export default interface IMedicalRecordRepo extends Repo<MedicalRecord>
{

    save(MedicalRecord: MedicalRecord): Promise<MedicalRecord>;
    update(MedicalRecord: MedicalRecord): Promise<MedicalRecord>;
    findByDescription(description: string): Promise<MedicalRecord>;
    findByDesignation(designation: string): Promise<MedicalRecord[]>;
    findAll(): Promise<MedicalRecord[]>;
    findById(id: MedicalRecordId | string): Promise<MedicalRecord>;
    findByMedicalConditionCode(medicalConditionCode: string): Promise<MedicalRecord>;

}