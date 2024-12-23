import { Container, Service, Inject } from "typedi";
import config from "../../config";

import IMedicalConditionService from "./IServices/IMedicalConditionService";
import { MedicalConditionMap } from "../mappers/MedicalConditionMap";
import { IMedicalConditionDTO } from "../dto/IMedicalConditionDTO";

import IMedicalConditionRepo from "./IRepos/IMedicalConditionRepo";

import { MedicalCondition } from "../domain/medicalCondition";

import { Result } from "../core/logic/Result";

@Service()
export default class MedicalConditionService implements IMedicalConditionService {
    constructor(
        @Inject(config.repos.medicalCondition.name) private medicalConditionRepo: IMedicalConditionRepo,
    ) { }


    public async createMedicalCondition(medicalConditionDTO: IMedicalConditionDTO): Promise<Result<IMedicalConditionDTO>> {

        try {

            const medicalConditionOrError = MedicalCondition.create(medicalConditionDTO);

            if (medicalConditionOrError.isFailure) {
                return Result.fail<IMedicalConditionDTO>(medicalConditionOrError.errorValue());
            }

            const medicalConditionResult = medicalConditionOrError.getValue();

            await this.medicalConditionRepo.save(medicalConditionResult);

            const medicalConditionDTOResult = MedicalConditionMap.toDTO(medicalConditionResult) as IMedicalConditionDTO;
            return Result.ok<IMedicalConditionDTO>(medicalConditionDTOResult)
        } catch (e) {
            throw e;
        }
    }

    public async UpdateMedicalCondition(medicalConditionDTO: IMedicalConditionDTO): Promise<Result<IMedicalConditionDTO>> {
        try {
            const medicalCondition = await this.medicalConditionRepo.findByMedicalConditionCode(medicalConditionDTO.medicalConditionCode);

            if (medicalCondition === null) {
                return Result.fail<IMedicalConditionDTO>("MedicalCondition not found");
            }
            else {
                if (medicalConditionDTO.designation != null) {
                    medicalCondition.designation = medicalConditionDTO.designation;
                }
                if (medicalConditionDTO.description != null) {
                    medicalCondition.description = medicalConditionDTO.description;
                }
                await this.medicalConditionRepo.save(medicalCondition);


                const medicalConditionDTOResult = MedicalConditionMap.toDTO(medicalCondition) as IMedicalConditionDTO;
                return Result.ok<IMedicalConditionDTO>(medicalConditionDTOResult)
            }
        } catch (e) {
            throw e;
        }
    }

    public async SearchMedicalCondition(medicalConditionCode?: string, designation?: string): Promise<Result<IMedicalConditionDTO[]>> {

        try {

            if (medicalConditionCode != null) {

                const medicalCondition = await this.medicalConditionRepo.findByMedicalConditionCode(medicalConditionCode);
                const medicalConditionDTO = MedicalConditionMap.toDTO(medicalCondition) as IMedicalConditionDTO;

                let medicalConditionDTOsArray = new Array<IMedicalConditionDTO>();
                medicalConditionDTOsArray.push(medicalConditionDTO);
                return Result.ok<IMedicalConditionDTO[]>(medicalConditionDTOsArray);

            }

            if (designation != null) {
                const medicalConditions = await this.medicalConditionRepo.findByDesignation(designation);

                const medicalConditionDTOs = medicalConditions.map((medicalCondition) => MedicalConditionMap.toDTO(medicalCondition) as IMedicalConditionDTO);

                return Result.ok<IMedicalConditionDTO[]>(medicalConditionDTOs);
            }
            else {

                const medicalConditions = await this.medicalConditionRepo.findAll();
                const medicalConditionDTOs = medicalConditions.map(medicalCondition => MedicalConditionMap.toDTO(medicalCondition) as IMedicalConditionDTO);
                return Result.ok<IMedicalConditionDTO[]>(medicalConditionDTOs);

            }

        } catch (e) {
            throw e;
        }

    }


}