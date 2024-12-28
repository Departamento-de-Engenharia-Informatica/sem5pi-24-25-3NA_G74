
import config from "../../config";
import IAllergyService from "./IServices/IAllergyService";
import IAllergyRepo from "./IRepos/IAllergyRepo";
import {IAllergyDTO} from "../dto/IAllergyDTO";
import {Result} from "../core/logic/Result";
import {Allergy} from "../domain/Allergy";
import {AllergyMap} from "../mappers/AllergyMap";
import {Inject, Service} from "typedi";
import {MedicalConditionMap} from "../mappers/MedicalConditionMap";
import {IMedicalConditionDTO} from "../dto/IMedicalConditionDTO";

@Service()
export default class AllergyService implements IAllergyService {
    constructor(
        @Inject(config.repos.allergy.name) private allergyRepo: IAllergyRepo,
    ) { }


    public async CreateAllergy(allergyDTO: IAllergyDTO): Promise<Result<IAllergyDTO>> {

        try {
            const allergyOrError = Allergy.create(allergyDTO);
            if (allergyOrError.isFailure) {
                return Result.fail<IAllergyDTO>(allergyOrError.errorValue());
            }
            const allergyResult = allergyOrError.getValue();

            await this.allergyRepo.save(allergyResult);

            const allergyDTOResult = AllergyMap.toDTO(allergyResult) as IAllergyDTO;
            return Result.ok<IAllergyDTO>(allergyDTOResult)
        } catch (e) {
            throw e;
        }
    }

    public async UpdateAllergy(allergyDTO: IAllergyDTO): Promise<Result<IAllergyDTO>> {
        try {
            const allergy = await this.allergyRepo.findByCode(allergyDTO.code);
            if (allergy === null) {
                return Result.fail<IAllergyDTO>("Allergy not found");
            }
            else {
                allergy.code = allergyDTO.code;
                allergy.designation = allergyDTO.designation;
                allergy.description = allergyDTO.description;
                await this.allergyRepo.save(allergy);
                const allergyDTOResult = AllergyMap.toDTO(allergy) as IAllergyDTO;
                return Result.ok<IAllergyDTO>(allergyDTOResult)
            }
        } catch (e) {
            throw e;
        }
    }

    public async SearchAllergy(code?: string, designation?: string): Promise<Result<IAllergyDTO[]>> {

        try {

            if (code != null) {

                const allergy = await this.allergyRepo.findByCode(code);
                const allergyDTO = AllergyMap.toDTO(allergy) as IAllergyDTO;

                let allergyDTOsArray = new Array<IAllergyDTO>();
                allergyDTOsArray.push(allergyDTO);
                return Result.ok<IAllergyDTO[]>(allergyDTOsArray);

            }

            if (designation != null) {
                const allergies = await this.allergyRepo.findByDesignation(designation);

                const allergyDTOs = allergies.map((allergy) => AllergyMap.toDTO(allergy) as IAllergyDTO);

                return Result.ok<IAllergyDTO[]>(allergyDTOs);
            }
            else {

                const allergies = await this.allergyRepo.findAll();
                const allergyDTOs = allergies.map(allergy => AllergyMap.toDTO(allergy) as IAllergyDTO);
                return Result.ok<IAllergyDTO[]>(allergyDTOs);

            }

        } catch (e) {
            throw e;
        }

    }


}