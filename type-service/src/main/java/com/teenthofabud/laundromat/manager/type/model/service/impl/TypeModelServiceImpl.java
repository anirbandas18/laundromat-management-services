package com.teenthofabud.laundromat.manager.type.model.service.impl;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.fge.jsonpatch.JsonPatch;
import com.github.fge.jsonpatch.JsonPatchException;
import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.core.common.service.TOABBaseService;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelMessageTemplate;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVException;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVVo;
import com.teenthofabud.laundromat.manager.type.lov.service.TypeLOVService;
import com.teenthofabud.laundromat.manager.type.model.converter.TypeModelDto2EntityConverter;
import com.teenthofabud.laundromat.manager.type.model.converter.TypeModelEntity2VoConverter;
import com.teenthofabud.laundromat.manager.type.model.converter.TypeModelForm2EntityConverter;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelDto;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelEntity;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelException;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelForm;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelVo;
import com.teenthofabud.laundromat.manager.type.model.mapper.TypeModelEntitySelfMapper;
import com.teenthofabud.laundromat.manager.type.model.mapper.TypeModelForm2EntityMapper;
import com.teenthofabud.laundromat.manager.type.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.type.model.repository.TypeModelRepository;
import com.teenthofabud.laundromat.manager.type.model.service.TypeModelService;
import com.teenthofabud.laundromat.manager.type.model.validator.TypeModelDtoValidator;
import com.teenthofabud.laundromat.manager.type.model.validator.TypeModelFormRelaxedValidator;
import com.teenthofabud.laundromat.manager.type.model.validator.TypeModelFormValidator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

@Component
@Slf4j
public class TypeModelServiceImpl implements TypeModelService {

    private static final Comparator<TypeModelVo> CMP_BY_NAME = (s1, s2) -> {
        return s1.getName().compareTo(s2.getName());
    };

    private TOABBaseService toabBaseService;
    private ObjectMapper om;
    private TypeLOVService typeLovService;

    private TypeModelEntity2VoConverter entity2VoConverter;
    private TypeModelForm2EntityConverter form2EntityConverter;
    private TypeModelDto2EntityConverter dto2EntityConverter;
    private TypeModelForm2EntityMapper form2EntityMapper;
    private TypeModelEntitySelfMapper entitySelfMapper;
    private TypeModelFormValidator formValidator;
    private TypeModelFormRelaxedValidator relaxedFormValidator;
    private TypeModelDtoValidator dtoValidator;
    private TypeModelRepository repository;

    @Autowired
    public void setRelaxedFormValidator(TypeModelFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setForm2EntityMapper(TypeModelForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(TypeModelEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

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
    public void setEntity2VoConverter(TypeModelEntity2VoConverter entity2VoConverter) {
        this.entity2VoConverter = entity2VoConverter;
    }

    @Autowired
    public void setDto2EntityConverter(TypeModelDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
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
    public void setTypeLovService(TypeLOVService typeLovService) {
        this.typeLovService = typeLovService;
    }

    @Autowired
    public void setFormValidator(TypeModelFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private List<TypeModelVo> entity2DetailedVoList(List<TypeModelEntity> typeModelEntityList) {
        List<TypeModelVo> typeModelDetailsList = new ArrayList<>(typeModelEntityList.size());
        for(TypeModelEntity entity : typeModelEntityList) {
            TypeModelVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            typeModelDetailsList.add(vo);
        }
        return typeModelDetailsList;
    }

    @Override
    @Transactional(readOnly = true)
    public Set<TypeModelVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all TypeModelEntity by their natural ordering");
        List<TypeModelEntity> typeModelEntityList = repository.findAll();
        Set<TypeModelVo> naturallyOrderedSet = new TreeSet<TypeModelVo>(CMP_BY_NAME);
        for(TypeModelEntity entity : typeModelEntityList) {
            TypeModelVo dto = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, dto);
            naturallyOrderedSet.add(dto);
        }
        log.info("{} TypeModelVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    @Transactional(readOnly = true)
    public TypeModelVo retrieveDetailsById(long id) throws TypeModelException {
        log.info("Requesting TypeModelEntity by id: {}", id);
        Optional<TypeModelEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug("No TypeModelEntity found by id: {}", id);
            throw new TypeModelException(TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        TypeModelEntity entity = optEntity.get();
        TypeModelVo vo = entity2VoConverter.convert(entity);
        log.info("Found TypeModelVo by id: {}", id);
        return vo;
    }

    @Override
    @Transactional(readOnly = true)
    public List<TypeModelVo> retrieveDetailsByTypeLOVId(long typeLovId) throws TypeModelException {
        log.info("Requesting TypeModelEntity that belong to typeLovId: {}", typeLovId);

        log.info("Requesting TypeLOVEntity by typeLovId: {}", typeLovId);
        TypeLOVVo typeLovVo = null;
        try {
            typeLovVo = typeLovService.retrieveDetailsById(typeLovId);
        } catch (TypeLOVException e) {
            log.debug("Unable to retrieve TypeLOVEntity with typeLovId: {}", typeLovId);
            throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "typeLovId", String.valueOf(typeLovId) });
        }
        log.debug("Retrieved TypeLOVEntity with typeLovId: {}", typeLovId);

        if(typeLovVo != null && !typeLovVo.getActive()) {
            log.debug("TypeLOVEntity is inactive with typeLovId: {}", typeLovId);
            throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "typeLovId", String.valueOf(typeLovId) });
        }
        log.debug("TypeLOVEntity is active with typeLovId: {}", typeLovId);

        List<TypeModelEntity> typeModelEntityList = repository.findByTypeLovId(typeLovId);
        if(typeModelEntityList != null && !typeModelEntityList.isEmpty()) {
            List<TypeModelVo> matchedTypeModelList = entity2DetailedVoList(typeModelEntityList);
            log.info("Found {} TypeModelVo belonging to typeLovId: {}", matchedTypeModelList.size(), typeLovId);
            return matchedTypeModelList;
        }
        log.debug("No TypeModelVo found belonging to typeLovId: {}", typeLovId);
        throw new TypeModelException(TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "typeLovId", String.valueOf(typeLovId) });

    }


    @Override
    @Transactional(readOnly = true)
    public List<TypeModelVo> retrieveAllMatchingDetailsByName(String name) throws TypeModelException {
        log.info("Requesting TypeModelEntity that match with name: {}", name);
        List<TypeModelEntity> typeModelEntityList = repository.findByNameContaining(name);
        if(typeModelEntityList != null && !typeModelEntityList.isEmpty()) {
            List<TypeModelVo> matchedTypeModelList = entity2DetailedVoList(typeModelEntityList);
            log.info("Found {} TypeModelVo matching with name: {}", matchedTypeModelList.size(),name);
            return matchedTypeModelList;
        }
        log.debug("No TypeModelVo found matching with name: {}", name);
        throw new TypeModelException(TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "name", name });
    }

    @Override
    @Transactional
    public Long createTypeModel(TypeModelForm form) throws TypeModelException {
        log.info("Creating new TypeModelEntity");

        if(form == null) {
            log.debug("TypeModelForm provided is null");
            throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of TypeModelForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("TypeModelForm has {} errors", err.getErrorCount());
            TypeErrorCode ec = TypeErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TypeModelForm error detail: {}", ec);
            throw new TypeModelException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of TypeModelForm are valid");

        log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_EXISTENCE_BY_NAME_AND_TYPE_LOV_ID, form.getName(), form.getTypeLovId());
        if(repository.existsByNameAndTypeLovId(form.getName(), form.getTypeLovId())) {
            log.debug("TypeModelEntity already exists with name: {} for typeLovId: {}", form.getName(), form.getTypeLovId());
            throw new TypeModelException(TypeErrorCode.TYPE_EXISTS,
                    new Object[]{ "name " + form.getName(), "typeLovId " + form.getTypeLovId() });
        }
        log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_NON_EXISTENCE_BY_NAME_AND_TYPE_LOV_ID, form.getName(), form.getTypeLovId());

        log.debug("Attempting to convert TypeModelForm to TypeModelEntity");
        TypeModelEntity expectedEntity = form2EntityConverter.convert(form);
        log.debug("Converted TypeModelForm to TypeModelEntity");

        log.debug("Saving {}", expectedEntity);
        TypeModelEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new TypeModelException(TypeErrorCode.TYPE_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist TypeModelForm details" });
        }
        log.info("Created new TypeModelForm with id: {}", actualEntity.getId());
        return actualEntity.getId();
    }

    @Override
    @Transactional
    public void updateTypeModel(Long id, TypeModelForm form) throws TypeModelException {
        log.info("Updating TypeModelForm by id: {}", id);

        log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TYPE_MODEL_ENTITY_ID, id);
        Optional<TypeModelEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_NO_TYPE_MODEL_ENTITY_ID_AVAILABLE, id);
            throw new TypeModelException(TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_FOUND_TYPE_MODEL_ENTITY_ID, id);

        TypeModelEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("TypeModelEntity is inactive with id: {}", id);
            throw new TypeModelException(TypeErrorCode.TYPE_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TypeModelEntity is active with id: {}", id);

        if(form == null) {
            log.debug("TypeModelForm is null");
            throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of TypeModelForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("TypeModelForm has {} errors", err.getErrorCount());
            TypeErrorCode ec = TypeErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TypeModelForm error detail: {}", ec);
            throw new TypeModelException(ec, new Object[] { err.getFieldError().getField() });
        } else if (!allEmpty) {
            log.debug("All attributes of TypeModelForm are empty");
            throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of TypeModelForm are valid");

        TypeModelEntity expectedEntity = null;

        try {
            Optional<TypeModelEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
            if(optExpectedEntity.isEmpty()) {
                log.debug("No new value for attributes of TypeLOVForm");
                throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
            }
            log.debug("Successfully compared and copied attributes from TypeModelForm to TypeModelEntity");
            expectedEntity = optExpectedEntity.get();
        } catch (TOABBaseException e) {
            throw (TypeModelException) e;
        }

        log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_EXISTENCE_BY_NAME_AND_TYPE_LOV_ID, expectedEntity.getName(), expectedEntity.getTypeLov().getId());
        if((actualEntity.getTypeLov().getId().equals(expectedEntity.getTypeLov().getId()) &&
                actualEntity.getName().compareTo(expectedEntity.getName()) == 0) ||
                repository.existsByNameAndTypeLovId(expectedEntity.getName(), expectedEntity.getTypeLov().getId())) {
            log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_EXISTS_BY_NAME_AND_TYPE_LOV_ID, expectedEntity.getName(), expectedEntity.getTypeLov().getId());
            throw new TypeModelException(TypeErrorCode.TYPE_EXISTS,
                    new Object[]{ "name", actualEntity.getName() });
        }
        log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_NON_EXISTENCE_BY_NAME_AND_TYPE_LOV_ID, expectedEntity.getName(), expectedEntity.getTypeLov().getId());

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from TypeModelEntity to TypeModelForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new TypeModelException(TypeErrorCode.TYPE_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency type LOV details" });
        }
        log.info("Updated existing TypeModelEntity with id: {} to version: {}", actualEntity.getId(), actualEntity.getVersion());
    }

    @Override
    @Transactional
    public void deleteTypeModel(Long id) throws TypeModelException {
        log.info("Soft deleting TypeModelEntity by id: {}", id);

        log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TYPE_MODEL_ENTITY_ID, id);
        Optional<TypeModelEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_NO_TYPE_MODEL_ENTITY_ID_AVAILABLE, id);
            throw new TypeModelException(TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_FOUND_TYPE_MODEL_ENTITY_ID, id);

        TypeModelEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("TypeModelEntity is inactive with id: {}", id);
            throw new TypeModelException(TypeErrorCode.TYPE_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TypeModelEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        TypeModelEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new TypeModelException(TypeErrorCode.TYPE_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current type LOV details with id:" + id });
        }

        log.info("Soft deleted existing TypeModelEntity with id: {}", actualEntity.getId());
    }

    @Override
    @Transactional
    public void applyPatchOnTypeModel(Long id, List<PatchOperationForm> patches) throws TypeModelException {
        log.info("Patching TypeModelEntity by id: {}", id);

        log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TYPE_MODEL_ENTITY_ID, id);
        Optional<TypeModelEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_NO_TYPE_MODEL_ENTITY_ID_AVAILABLE, id);
            throw new TypeModelException(TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_FOUND_TYPE_MODEL_ENTITY_ID, id);

        TypeModelEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("TypeModel patch list not provided");
            throw new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("TypeModel patch list has {} items", patches.size());


        log.debug("Validating patch list items for TypeModel");
        try {
            toabBaseService.validatePatches(patches, TypeErrorCode.TYPE_EXISTS.getDomain() + ":MODEL");
            log.debug("All TypeModel patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the TypeModel patch item are invalid");
            throw new TypeModelException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for TypeModel");


        log.debug("Patching list items to TypeModelDto");
        TypeModelDto patchedTypeModelForm = new TypeModelDto();
        try {
            log.debug("Preparing patch list items for TypeModel");
            JsonNode typeModelDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch typeModelPatch = JsonPatch.fromJson(typeModelDtoTree);
            log.debug("Prepared patch list items for TypeModel");
            JsonNode blankTypeModelDtoTree = om.convertValue(new TypeModelDto(), JsonNode.class);
            JsonNode patchedTypeModelFormTree = typeModelPatch.apply(blankTypeModelDtoTree);
            log.debug("Applying patch list items to TypeModelDto");
            patchedTypeModelForm = om.treeToValue(patchedTypeModelFormTree, TypeModelDto.class);
            log.debug("Applied patch list items to TypeModelDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to TypeLOVDto: {}", e);
            TypeModelException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in TypeModelDto");
                ex = new TypeModelException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new TypeModelException(TypeErrorCode.TYPE_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to TypeModelDto: {}", e);
            throw new TypeModelException(TypeErrorCode.TYPE_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to TypeModelDto");

        log.debug("Validating patched TypeModelForm");
        Errors err = new DirectFieldBindingResult(patchedTypeModelForm, patchedTypeModelForm.getClass().getSimpleName());
        dtoValidator.validate(patchedTypeModelForm, err);
        if(err.hasErrors()) {
            log.debug("Patched TypeModelForm has {} errors", err.getErrorCount());
            TypeErrorCode ec = TypeErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched TypeModelForm error detail: {}", ec);
            throw new TypeModelException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched TypeModelForm are valid");

        checkUniquenessOfTypeModelPatch(patchedTypeModelForm, actualEntity);

        log.debug("Comparatively copying patched attributes from TypeModelDto to TypeModelEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedTypeModelForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (TypeModelException) e;
        }
        log.debug("Comparatively copied patched attributes from TypeModelDto to TypeModelEntity");

        log.debug("Saving patched TypeModelEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched TypeModelEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete TypeModelEntity with id:{}", id);
            throw new TypeModelException(TypeErrorCode.TYPE_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency type LOV details with id:" + id });
        }
        log.info("Patched TypeModelEntity with id:{}", id);
    }

    private void checkUniquenessOfTypeModelPatch(TypeModelDto patchedTypeModelForm, TypeModelEntity actualEntity) throws TypeModelException {
        if(patchedTypeModelForm.getTypeLovId().isPresent() && patchedTypeModelForm.getName().isPresent()) {
            log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_EXISTENCE_BY_NAME_AND_TYPE_LOV_ID,
                    patchedTypeModelForm.getName().get(), patchedTypeModelForm.getTypeLovId().get());
            boolean sameEntitySw = patchedTypeModelForm.getTypeLovId().get().equals(actualEntity.getTypeLov().getId().toString())
                    && patchedTypeModelForm.getName().get().equals(actualEntity.getName());
            boolean duplicateEntitySw =  repository.existsByNameAndTypeLovId(
                    patchedTypeModelForm.getName().get(), Long.parseLong(patchedTypeModelForm.getTypeLovId().get()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_EXISTS_BY_NAME_AND_TYPE_LOV_ID,
                        patchedTypeModelForm.getName().get(), patchedTypeModelForm.getTypeLovId().get());
                throw new TypeModelException(TypeErrorCode.TYPE_EXISTS,
                        new Object[]{ "name", actualEntity.getName(), "typeLovId", actualEntity.getTypeLov().getId()});
            }
            log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_NON_EXISTENCE_BY_NAME_AND_TYPE_LOV_ID,
                    patchedTypeModelForm.getName().get(), patchedTypeModelForm.getTypeLovId().get());
        }

        if(patchedTypeModelForm.getTypeLovId().isPresent() && patchedTypeModelForm.getName().isEmpty()) {
            log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_EXISTENCE_BY_NAME_AND_TYPE_LOV_ID,
                    patchedTypeModelForm.getTypeLovId().get(), actualEntity.getName());
            boolean sameEntitySw = patchedTypeModelForm.getTypeLovId().get().equals(actualEntity.getTypeLov().getId().toString());
            boolean duplicateEntitySw =  repository.existsByNameAndTypeLovId(actualEntity.getName(), Long.parseLong(patchedTypeModelForm.getTypeLovId().get()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_EXISTS_BY_NAME_AND_TYPE_LOV_ID,
                        actualEntity.getName(), patchedTypeModelForm.getTypeLovId().get());
                throw new TypeModelException(TypeErrorCode.TYPE_EXISTS,
                        new Object[]{ "name", actualEntity.getName(), "typeLovId", patchedTypeModelForm.getTypeLovId().get() });
            }
            log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_NON_EXISTENCE_BY_NAME_AND_TYPE_LOV_ID, actualEntity.getName(), patchedTypeModelForm.getTypeLovId().get());
        }

        if(patchedTypeModelForm.getTypeLovId().isEmpty() && patchedTypeModelForm.getName().isPresent()) {
            log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_EXISTENCE_BY_NAME_AND_TYPE_LOV_ID,
                    patchedTypeModelForm.getName().get(), actualEntity.getTypeLov().getId());
            boolean sameEntitySw = patchedTypeModelForm.getName().get().equals(actualEntity.getName());
            boolean duplicateEntitySw =  repository.existsByNameAndTypeLovId(patchedTypeModelForm.getName().get(), actualEntity.getTypeLov().getId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_EXISTS_BY_NAME_AND_TYPE_LOV_ID,
                        patchedTypeModelForm.getName().get(), actualEntity.getTypeLov().getId());
                throw new TypeModelException(TypeErrorCode.TYPE_EXISTS,
                        new Object[]{ "name", actualEntity.getName(), "typeLovId", actualEntity.getTypeLov().getId() });
            }
            log.debug(TypeModelMessageTemplate.MSG_TEMPLATE_TYPE_MODEL_NON_EXISTENCE_BY_NAME_AND_TYPE_LOV_ID, patchedTypeModelForm.getName().get(), actualEntity.getTypeLov().getId());
        }
    }
}