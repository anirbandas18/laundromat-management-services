package com.teenthofabud.laundromat.manager.type.service.impl;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.fge.jsonpatch.JsonPatch;
import com.github.fge.jsonpatch.JsonPatchException;
import com.teenthofabud.core.common.converter.ComparativeFormConverter;
import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.model.error.TOABBaseException;
import com.teenthofabud.core.common.model.form.PatchOperationForm;
import com.teenthofabud.core.common.service.TOABBaseService;
import com.teenthofabud.core.common.validator.RelaxedValidator;
import com.teenthofabud.laundromat.manager.type.converter.entity2vo.TypeModelEntity2VoConverter;
import com.teenthofabud.laundromat.manager.type.converter.form2entity.TypeModelForm2EntityConverter;
import com.teenthofabud.laundromat.manager.type.model.constants.LOVType;
import com.teenthofabud.laundromat.manager.type.model.dto.TypeModelDto;
import com.teenthofabud.laundromat.manager.type.model.entity.TypeModelEntity;
import com.teenthofabud.laundromat.manager.type.model.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.type.model.error.TypeException;
import com.teenthofabud.laundromat.manager.type.model.form.TypeModelForm;
import com.teenthofabud.laundromat.manager.type.model.vo.TypeModelVo;
import com.teenthofabud.laundromat.manager.type.repository.TypeLOVRepository;
import com.teenthofabud.laundromat.manager.type.repository.TypeModelRepository;
import com.teenthofabud.laundromat.manager.type.service.TypeModelService;
import com.teenthofabud.laundromat.manager.type.validator.dto.TypeModelDtoValidator;
import com.teenthofabud.laundromat.manager.type.validator.form.TypeModelFormValidator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import javax.annotation.PostConstruct;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;

@Component
@Slf4j
public class TypeModelServiceImpl implements TypeModelService {

    private static final Comparator<TypeModelVo> CMP_BY_NAME = (s1, s2) -> {
        return s1.getName().compareTo(s2.getName());
    };

    private static RelaxedValidator<TypeModelForm> LOOSE_CURRENCY_TYPE_VALIDATOR;
    private static ComparativePatchConverter<TypeModelEntity, TypeModelDto> COMPARE_AND_COPY_PATCH;
    private static ComparativeFormConverter<TypeModelEntity, TypeModelEntity> COMPARE_AND_COPY_ENTITY;
    private static ComparativeFormConverter<TypeModelEntity, TypeModelForm> COMPARE_AND_COPY_FORM;

    private TypeModelEntity2VoConverter entity2VoConverter;
    private TypeModelForm2EntityConverter form2EntityConverter;
    private TypeModelFormValidator formValidator;
    private TypeModelDtoValidator dtoValidator;
    private TypeModelRepository repository;
    private TypeLOVRepository typeRepository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;

    @Autowired
    public void setPatchOperationValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(TypeModelDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setEntity2VOConverter(TypeModelEntity2VoConverter entity2VoConverter) {
        this.entity2VoConverter = entity2VoConverter;
    }

    @Autowired
    public void setForm2EntityConverter(TypeModelForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(TypeModelRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setTypeRepository(TypeLOVRepository typeRepository) {
        this.typeRepository = typeRepository;
    }

    @Autowired
    public void setFormValidator(TypeModelFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    @Override
    @PostConstruct
    public void init() {
        LOOSE_CURRENCY_TYPE_VALIDATOR = (form, errors) -> {
            if(form.getName() != null && form.getName().length() == 0) {
                errors.rejectValue("name", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
                log.debug("TypeModelForm.name is empty");
                return false;
            }
            log.debug("TypeModelForm.name is valid");
            return true;
        };

        COMPARE_AND_COPY_PATCH = (actualEntity, form) -> {
            boolean changeSW = false;
            if(form.getDescription().isPresent()) {
                actualEntity.setDescription(form.getDescription().get());
                changeSW = true;
                log.debug("TypeModelDto.description is valid");
            }
            if(form.getName().isPresent()) {
                actualEntity.setName(form.getName().get());
                changeSW = true;
                log.debug("TypeModelDto.name is valid");
            }
            if(form.getActive().isPresent()) {
                actualEntity.setActive(Boolean.valueOf(form.getActive().get()));
                changeSW = true;
                log.debug("TypeModelDto.active is valid");
            }
            if(changeSW) {
                log.debug("All provided TypeModelDto attributes are valid");
                actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
                return;
            }
            log.debug("Not all provided TypeModelDto attributes are valid");
        };

        COMPARE_AND_COPY_ENTITY = (source, target) -> {
            TypeModelEntity expectedEntity = new TypeModelEntity();
            boolean changeSW = false;
            if(source.getId() != null && source.getId() > 0 && source.getId().compareTo(target.getId()) != 0) {
                target.setId(source.getId());
                changeSW = true;
                log.debug("Source TypeModelEntity.id is valid");
            }
            if(source.getDescription() != null && source.getDescription().compareTo(target.getDescription()) != 0) {
                target.setDescription(source.getDescription());
                changeSW = true;
                log.debug("Source TypeModelEntity.description is valid");
            }
            if(source.getName() != null && StringUtils.hasText(source.getName()) && source.getName().compareTo(target.getName()) != 0) {
                target.setName(source.getName());
                changeSW = true;
                log.debug("Source TypeModelEntity.name is valid");
            }
            if(changeSW) {
                log.debug("All provided TypeModelEntity attributes are valid");
                return Optional.of(target);
            } else {
                log.debug("Not all provided TypeModelEntity attributes are valid");
                return Optional.empty();
            }
        };

        COMPARE_AND_COPY_FORM = (actualEntity, form) -> {
            TypeModelEntity expectedEntity = new TypeModelEntity();
            boolean changeSW = false;
            // direct copy
            expectedEntity.setId(actualEntity.getId());
            log.debug("Directly copying TypeModelEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
            expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
            log.debug("Directly copying TypeModelEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
            expectedEntity.setActive(actualEntity.getActive());
            log.debug("Directly copying TypeModelEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
            // comparative copy
            if(StringUtils.hasText(form.getName()) && form.getName().compareTo(actualEntity.getName()) != 0) {
                expectedEntity.setName(form.getName());
                changeSW = true;
                log.debug("TypeModelForm.name: {} is same as TypeModelEntity.name: {}", form.getName(), actualEntity.getName());
            } else {
                expectedEntity.setName(actualEntity.getName());
                changeSW = true;
                log.debug("TypeModelForm.name: {} is different to TypeModelEntity.name: {}", form.getName(), actualEntity.getName());
            }
            if(StringUtils.hasText(form.getDescription()) &&
                    form.getDescription().toLowerCase().compareTo(actualEntity.getDescription().toLowerCase()) != 0) {
                expectedEntity.setDescription(form.getDescription());
                changeSW = true;
                log.debug("TypeModelForm.description: {} is same as TypeModelEntity.description: {}", form.getDescription(), actualEntity.getDescription());
            } else {
                expectedEntity.setDescription(actualEntity.getDescription());
                changeSW = true;
                log.debug("TypeModelForm.description: {} is different to TypeModelEntity.description: {}", form.getDescription(), actualEntity.getDescription());
            }
            return changeSW ? Optional.of(expectedEntity) : Optional.empty();
        };
        log.debug("Initialized bean with pre-requisites");
    }

    private List<TypeModelVo> entity2DetailedVoList(List<TypeModelEntity> studentEntityList) {
        List<TypeModelVo> studentDetailsList = new ArrayList<>(studentEntityList.size());
        for(TypeModelEntity entity : studentEntityList) {
            TypeModelVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            studentDetailsList.add(vo);
        }
        return studentDetailsList;
    }

    @Override
    @Transactional(readOnly = true)
    public Set<TypeModelVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all TypeModelEntity by their natural ordering");
        List<TypeModelEntity> studentEntityList = repository.findAll();
        Set<TypeModelVo> naturallyOrderedSet = new TreeSet<TypeModelVo>();
        for(TypeModelEntity entity : studentEntityList) {
            TypeModelVo dto = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, dto);
            naturallyOrderedSet.add(dto);
        }
        log.info("{} TypeModelVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    @Transactional(readOnly = true)
    public TypeModelVo retrieveDetailsById(long id) throws TypeException {
        log.info("Requesting TypeModelEntity by id: {}", id);
        Optional<TypeModelEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug("No TypeModelEntity found by id: {}", id);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        TypeModelEntity entity = optEntity.get();
        if(!entity.getActive()) {
            log.debug("TypeModelEntity is inactive by id: {}", id);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_INACTIVE, new Object[] { String.valueOf(id) });
        }
        TypeModelVo vo = entity2VoConverter.convert(entity);
        log.info("Found TypeModelVo by id: {}", id);
        return vo;
    }

    @Override
    @Transactional(readOnly = true)
    public List<TypeModelVo> retrieveDetailsByTypeLOVId(long typeLovId) throws TypeException {
        log.info("Requesting TypeModelEntity that belong to typeLovId: {}", typeLovId);

        log.info("Requesting TypeLOVEntity by typeLovId: {}", typeLovId);
        Boolean exists = typeRepository.existsById(typeLovId);
        if(!exists) {
            log.debug("No TypeLOVEntity found by typeLovId: {}", typeLovId);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "typeLovId", String.valueOf(typeLovId) });
        }

        List<TypeModelEntity> studentEntityList = repository.findByTypeLovId(typeLovId);
        if(studentEntityList != null && !studentEntityList.isEmpty()) {
            List<TypeModelVo> matchedTypeModelList = entity2DetailedVoList(studentEntityList);
            log.info("Found {} TypeModelVo belonging to typeLovId: {}", matchedTypeModelList.size(), typeLovId);
            return matchedTypeModelList;
        }
        log.debug("No TypeModelVo found belonging to typeLovId: {}", typeLovId);
        throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "typeLovId", typeLovId });

    }


    @Override
    @Transactional(readOnly = true)
    public List<TypeModelVo> retrieveAllMatchingDetailsByName(String name) throws TypeException {
        log.info("Requesting TypeModelEntity that match with name: {}", name);
        List<TypeModelEntity> studentEntityList = repository.findByNameContaining(name);
        if(studentEntityList != null && !studentEntityList.isEmpty()) {
            List<TypeModelVo> matchedTypeModelList = entity2DetailedVoList(studentEntityList);
            log.info("Found {} TypeModelVo matching with name: {}", matchedTypeModelList.size(),name);
            return matchedTypeModelList;
        }
        log.debug("No TypeModelVo found matching with name: {}", name);
        throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "name", name });
    }

    @Override
    @Transactional
    public Long createTypeModel(TypeModelForm form) throws TypeException {
        log.info("Creating new TypeModelEntity");

        if(form == null) {
            log.debug("TypeModelForm provided is null");
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "not provided" });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of TypeModelForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("TypeModelForm has {} errors", err.getErrorCount());
            TypeErrorCode ec = TypeErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TypeModelForm error detail: {}", ec);
            throw new TypeException(LOVType.CURRENCY_TYPE, ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getDefaultMessage() });
        }
        log.debug("All attributes of TypeModelForm are valid");

        log.debug("Checking existence of TypeModelEntity with name: {}", form.getName());
        TypeModelEntity expectedEntity = form2EntityConverter.convert(form);
        if(repository.existsByName(expectedEntity.getName())) {
            log.debug("TypeModelEntity already exists with name: {}", expectedEntity.getName());
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_EXISTS,
                    new Object[]{ "name", form.getName() });
        }
        log.debug("No TypeModelEntity exists with name: {}", expectedEntity.getName());

        log.debug("Saving {}", expectedEntity);
        TypeModelEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist TypeModelForm details" });
        }
        log.info("Created new TypeModelForm with id: {}", actualEntity.getId());
        return actualEntity.getId();
    }

    @Override
    @Transactional
    public void updateTypeModel(Long id, TypeModelForm form) throws TypeException {
        log.info("Updating TypeModelForm by id: {}", id);

        log.debug("Searching for TypeModelEntity with id: {}", id);
        Optional<TypeModelEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug("No TypeModelEntity available with id: {}", id);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug("Found TypeModelEntity with id: {}", id);

        TypeModelEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("TypeModelEntity is inactive with id: {}", id);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TypeModelEntity is active with id: {}", id);

        if(form == null) {
            log.debug("TypeModelForm is null");
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "not provided" });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of TypeModelForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = LOOSE_CURRENCY_TYPE_VALIDATOR.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("TypeModelForm has {} errors", err.getErrorCount());
            TypeErrorCode ec = TypeErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TypeModelForm error detail: {}", ec);
            throw new TypeException(LOVType.CURRENCY_TYPE, ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getDefaultMessage() });
        } else if (!allEmpty) {
            log.debug("All attributes of TypeModelForm are empty");
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of TypeModelForm are valid");

        Optional<TypeModelEntity> optExpectedEntity = COMPARE_AND_COPY_FORM.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("All attributes of TypeModelForm are empty");
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("Successfully compared and copied attributes from TypeModelForm to TypeModelEntity");

        log.debug("Checking existence of TypeModelEntity with name: {}", form.getName());
        TypeModelEntity expectedEntity = optExpectedEntity.get();
        if(repository.existsByName(expectedEntity.getName())) {
            log.debug("TypeModelEntity already exists with name: {}", expectedEntity.getName());
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_EXISTS,
                    new Object[]{ "name", actualEntity.getName() });
        }
        log.debug("No TypeModelEntity exists with name: {}", expectedEntity.getName());

        COMPARE_AND_COPY_ENTITY.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from TypeModelEntity to TypeModelForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency type LOV details" });
        }
        log.info("Updated existing TypeModelEntity with id: {} to version: {}", actualEntity.getId(), actualEntity.getVersion());
    }

    @Override
    @Transactional
    public void deleteTypeModel(Long id) throws TypeException {
        log.info("Soft deleting TypeModelEntity by id: {}", id);

        log.debug("Searching for TypeModelEntity with id: {}", id);
        Optional<TypeModelEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug("No TypeModelEntity available with id: {}", id);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug("Found TypeModelEntity with id: {}", id);

        TypeModelEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("TypeModelEntity is inactive with id: {}", id);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TypeModelEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        TypeModelEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current type LOV details with id:" + id });
        }

        log.info("Soft deleted existing TypeModelEntity with id: {}", actualEntity.getId());
    }

    @Override
    @Transactional
    public void applyPatchOnTypeModel(Long id, List<PatchOperationForm> patches) throws TypeException {
        log.info("Patching TypeModelEntity by id: {}", id);

        log.debug("Searching for TypeModelEntity with id: {}", id);
        Optional<TypeModelEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug("No TypeModelEntity available with id: {}", id);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug("Found TypeModelEntity with id: {}", id);

        TypeModelEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("TypeModel patch list not provided");
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", "not provided" });
        }
        log.debug("TypeModel patch list has {} items", patches.size());


        log.debug("Validating patch list items for TypeModel");
        try {
            toabBaseService.validatePatches(patches, TypeErrorCode.TYPE_EXISTS.getDomain() + ":" + LOVType.CURRENCY_TYPE.name());
            log.debug("All TypeModel patch list items are valid");
        } catch (TOABBaseException e) {
            log.debug("Some of the TypeModel patch item are invalid");
            throw new TypeException(LOVType.CURRENCY_TYPE, e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for TypeModel");


        log.debug("Patching list items to TypeModelDto");
        TypeModelDto patchedTypeModelForm = new TypeModelDto();
        try {
            log.debug("Preparing patch list items for TypeModel");
            JsonNode studentDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch studentPatch = JsonPatch.fromJson(studentDtoTree);
            log.debug("Prepared patch list items for TypeModel");
            JsonNode blankTypeModelDtoTree = om.convertValue(new TypeModelDto(), JsonNode.class);
            JsonNode patchedTypeModelFormTree = studentPatch.apply(blankTypeModelDtoTree);
            log.debug("Applying patch list items to TypeModelDto");
            patchedTypeModelForm = om.treeToValue(patchedTypeModelFormTree, TypeModelDto.class);
            log.debug("Applied patch list items to TypeModelDto");
        } catch (IOException | JsonPatchException e) {
            log.debug("Failed to patch list items to TypeModelDto: {}", e);
            e.printStackTrace();
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to TypeModelDto");

        log.debug("Validating patched TypeModelForm");
        Errors err = new DirectFieldBindingResult(patchedTypeModelForm, patchedTypeModelForm.getClass().getSimpleName());
        dtoValidator.validate(patchedTypeModelForm, err);
        if(err.hasErrors()) {
            log.debug("Patched TypeModelForm has {} errors", err.getErrorCount());
            TypeErrorCode ec = TypeErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched TypeModelForm error detail: {}", ec);
            throw new TypeException(LOVType.CURRENCY_TYPE, ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getDefaultMessage() });
        }
        log.debug("All attributes of patched TypeModelForm are valid");

        log.debug("Comparatively copying patched attributes from TypeModelDto to TypeModelEntity");
        COMPARE_AND_COPY_PATCH.compareAndMap(actualEntity, patchedTypeModelForm);
        log.debug("Comparatively copied patched attributes from TypeModelDto to TypeModelEntity");

        log.debug("Saving patched TypeModelEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched TypeModelEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete TypeModelEntity with id:{}", id);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency type LOV details with id:" + id });
        }
        log.info("Patched TypeModelEntity with id:{}", id);
    }
}