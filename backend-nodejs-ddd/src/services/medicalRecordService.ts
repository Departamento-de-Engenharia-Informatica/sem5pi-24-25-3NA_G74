import { Inject, Service } from "typedi";
import config from "../../config";
import IMedicalRecordRepo from "./IRepos/IMedicalRecordRepo";
import IMedicalRecordService from "./IServices/IMedcialRecordService";
import { Result } from "../core/logic/Result";
import IMedicalRecordDTO from "../dto/IMedicalRecordDTO";




@Service()
export default class MedicalRecordService implements IMedicalRecordService
{
    constructor(
        @Inject(config.repos.medicalRecord.name) private medicalRecordRepo: IMedicalRecordRepo,
    ) { }
    createMedicalRecord(medicalRecordDTO: IMedicalRecordDTO): Promise<Result<IMedicalRecordDTO>> {
        throw new Error("Method not implemented.");
    }
    UpdateMedicalRecord(medicalRecordDTO: IMedicalRecordDTO): Promise<Result<IMedicalRecordDTO>> {
        throw new Error("Method not implemented.");
    }
    SearchMedicalRecord(medicalRecordCode?: string, designation?: string): Promise<Result<IMedicalRecordDTO[]>> {
        throw new Error("Method not implemented.");
    }
}