import { Service, Inject } from 'typedi';

import { Document, Model } from 'mongoose';
import {IAllergyPersistence} from "../dataschema/IAllergyPersistence";
import {AllergyId} from "../domain/AllergyId";
import {Allergy} from "../domain/Allergy";
import {AllergyMap} from "../mappers/AllergyMap";
import {MedicalCondition} from "../domain/medicalCondition";
import {MedicalConditionMap} from "../mappers/MedicalConditionMap";
import IAllergyRepo from '../services/IRepos/IAllergyRepo';

@Service()
export default class AllergyRepo implements IAllergyRepo {
    private models: any;

    constructor(
        @Inject('allergySchema') private allergySchema : Model<IAllergyPersistence & Document>,
        @Inject('logger') private logger
    ) { }

    public async exists (allergyId: AllergyId | string): Promise<boolean> {

        const idX = allergyId instanceof AllergyId ? (<AllergyId>allergyId).id.toValue() : allergyId;

        const query = { domainId: idX};
        const allergyDocument = await this.allergySchema.findOne( query );

        return !!allergyDocument;
    }

    public async save (allergy: Allergy): Promise<Allergy> {
        const query = { domainId: allergy.id.toString() };

        const allergyDocument = await this.allergySchema.findOne( query );

        try {
            if (allergyDocument === null ) {
                const rawAllergy: any = AllergyMap.toPersistence(allergy);

                const allergyCreated = await this.allergySchema.create(rawAllergy);

                return AllergyMap.toDomain(allergyCreated);
            } else {
                allergyDocument.code = allergy.code
                allergyDocument.designation = allergy.designation;
                allergyDocument.description = allergy.description;
                await allergyDocument.save();

                return allergy;
            }
        } catch (err) {
            throw err;
        }
    }

    public async findById (allergyId: AllergyId | string): Promise<Allergy> {

        const idX = allergyId instanceof AllergyId ? (<AllergyId>allergyId).id.toValue() : allergyId;

        const query = { domainId: idX };
        const allergyRecord = await this.allergySchema.findOne( query );

        if( allergyRecord != null) {
            return AllergyMap.toDomain(allergyRecord);
        }
        else
            return null;
    }

    public async update(allergy: Allergy): Promise<Allergy> {

        const query = { domainId: allergy.id.toString() };
        const allergyDocument = await this.allergySchema.findOne(query);

        if (allergyDocument === null) {
            throw Error("Allergy not found, couldn't update");
        } else {
            allergyDocument.code = allergy.code;
            allergyDocument.designation = allergy.designation;
            allergyDocument.description = allergy.description;
            await allergyDocument.save();

            return allergy;
        }
    }

    public async findByCode(code: string): Promise<Allergy> {

        const query = { code: code.toString() };
        const allergyRecord = await this.allergySchema.findOne(query)

        if (allergyRecord != null) {
            return AllergyMap.toDomain(allergyRecord);
        }
        else {
            throw Error("Allergy not found");
        }
    }

    public async findByDesignation(designation: string): Promise<Allergy> {

        const query = { designation: designation.toString() };
        const allergyRecord = await this.allergySchema.findOne(query)

        if (allergyRecord != null) {
            return AllergyMap.toDomain(allergyRecord);
        }
        else {
            throw Error("Allergy not found");
        }
    }

    public async findAll(): Promise<Allergy[]> {

        const allergyRecord = await this.allergySchema.find();

        if (allergyRecord != null) {
            return allergyRecord.map((allergyRecord) => AllergyMap.toDomain(allergyRecord));
        } else {
            throw Error("Allergy not found");
        }

    }

    public async findByDescription(description: string): Promise<Allergy> {

        const query = { description: description.toString() };
        const allergyRecord = await this.allergySchema.findOne(query)

        if (allergyRecord != null) {
            return AllergyMap.toDomain(allergyRecord);
        }
        else {
            throw Error("Allergy not found");
        }
    }
}