package com.teenthofabud.laundromat.manager.type.lov.service.impl;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.fge.jsonpatch.JsonPatch;
import com.github.fge.jsonpatch.JsonPatchException;
import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.core.common.service.TOABBaseService;
import com.teenthofabud.laundromat.manager.type.lov.converter.TypeLOVDto2EntityConverter;
import com.teenthofabud.laundromat.manager.type.lov.converter.TypeLOVForm2EntityConverter;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVDto;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVEntity;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVException;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVForm;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVMessageTemplate;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVVo;
import com.teenthofabud.laundromat.manager.type.lov.mapper.TypeLOVEntitySelfMapper;
import com.teenthofabud.laundromat.manager.type.lov.mapper.TypeLOVForm2EntityMapper;
import com.teenthofabud.laundromat.manager.type.lov.repository.TypeLOVRepository;
import com.teenthofabud.laundromat.manager.type.lov.service.TypeLOVService;
import com.teenthofabud.laundromat.manager.type.lov.validator.TypeLOVDtoValidator;
import com.teenthofabud.laundromat.manager.type.lov.validator.TypeLOVFormRelaxedValidator;
import com.teenthofabud.laundromat.manager.type.lov.validator.TypeLOVFormValidator;
import com.teenthofabud.laundromat.manager.type.lov.converter.TypeLOVEntity2VoConverter;
import com.teenthofabud.laundromat.manager.type.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelEntity;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelException;
import com.teenthofabud.laundromat.manager.type.model.service.TypeModelService;
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
public class TypeLOVServiceImpl implements TypeLOVService {

    private static final Comparator<TypeLOVVo> CMP_BY_NAME = (s1, s2) -> {
        return s1.getName().compareTo(s2.getName());
    };

    private TypeLOVEntity2VoConverter entity2VoConverter;
    private TypeLOVForm2EntityConverter form2EntityConverter;
    private TypeLOVDto2EntityConverter dto2EntityConverter;
    private TypeLOVForm2EntityMapper form2EntityMapper;
    private TypeLOVEntitySelfMapper entitySelfMapper;
    private TypeLOVFormValidator formValidator;
    private TypeLOVFormRelaxedValidator relaxedFormValidator;
    private TypeLOVDtoValidator dtoValidator;
    private TypeLOVRepository repository;
    private TOABBaseService toabBaseService;
    private TypeModelService typeModelService;
    private ObjectMapper om;

    @Autowired
    public void setTypeModelService(TypeModelService typeModelService) {
        this.typeModelService = typeModelService;
    }

    @Autowired
    public void setEntity2VoConverter(TypeLOVEntity2VoConverter entity2VoConverter) {
        this.entity2VoConverter = entity2VoConverter;
    }

    @Autowired
    public void setDto2EntityConverter(TypeLOVDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(TypeLOVForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(TypeLOVEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(TypeLOVFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
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
    public void setDtoValidator(TypeLOVDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2EntityConverter(TypeLOVForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(TypeLOVRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(TypeLOVFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private List<TypeLOVVo> entity2DetailedVoList(List<TypeLOVEntity> typeLovEntityList) {
        List<TypeLOVVo> typeLovDetailsList = new ArrayList<>(typeLovEntityList.size());
        for(TypeLOVEntity entity : typeLovEntityList) {
            TypeLOVVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            typeLovDetailsList.add(vo);
        }
        return typeLovDetailsList;
    }

    @Override
    @Transactional(readOnly = true)
    public Set<TypeLOVVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all TypeLOVEntity by their natural ordering");
        List<TypeLOVEntity> typeLovEntityList = repository.findAll();
        Set<TypeLOVVo> naturallyOrderedSet = new TreeSet<TypeLOVVo>(CMP_BY_NAME);
        for(TypeLOVEntity entity : typeLovEntityList) {
            TypeLOVVo dto = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, dto);
            naturallyOrderedSet.add(dto);
        }
        log.info("{} TypeLOVVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    @Transactional(readOnly = true)
    public TypeLOVVo retrieveDetailsById(long id) throws TypeLOVException {
        log.info("Requesting TypeLOVEntity by id: {}", id);
        Optional<TypeLOVEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug("No TypeLOVEntity found by id: {}", id);
            throw new TypeLOVException(TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        TypeLOVEntity entity = optEntity.get();
        TypeLOVVo vo = entity2VoConverter.convert(entity);
        log.info("Found TypeLOVVo by id: {}", id);
        return vo;
    }


    @Override
    @Transactional(readOnly = true)
    public List<TypeLOVVo> retrieveAllMatchingDetailsByName(String name) throws TypeLOVException {
        log.info("Requesting TypeLOVEntity that match with name: {}", name);
        List<TypeLOVEntity> typeLovEntityList = repository.findByNameContaining(name);
        if(typeLovEntityList != null && !typeLovEntityList.isEmpty()) {
            List<TypeLOVVo> matchedTypeLOVList = entity2DetailedVoList(typeLovEntityList);
            log.info("Found {} TypeLOVVo matching with name: {}", matchedTypeLOVList.size(),name);
            return matchedTypeLOVList;
        }
        log.debug("No TypeLOVVo found matching with name: {}", name);
        throw new TypeLOVException(TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "name", name });
    }

    @Override
    @Transactional
    public Long createTypeLOV(TypeLOVForm form) throws TypeLOVException {
        log.info("Creating new TypeLOVEntity");

        if(form == null) {
            log.debug("TypeLOVForm provided is null");
            throw new TypeLOVException(TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of TypeLOVForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("TypeLOVForm has {} errors", err.getErrorCount());
            TypeErrorCode ec = TypeErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TypeLOVForm error detail: {}", ec);
            throw new TypeLOVException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of TypeLOVForm are valid");

        log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_EXISTENCE_BY_NAME.getValue(), form.getName());
        TypeLOVEntity expectedEntity = form2EntityConverter.convert(form);
        if(repository.existsByName(expectedEntity.getName())) {
            log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_EXISTS_BY_NAME.getValue(), expectedEntity.getName());
            throw new TypeLOVException(TypeErrorCode.TYPE_EXISTS,
                    new Object[]{ "name", form.getName() });
        }
        log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_NON_EXISTENCE_BY_NAME.getValue(), expectedEntity.getName());

        log.debug("Saving {}", expectedEntity);
        TypeLOVEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new TypeLOVException(TypeErrorCode.TYPE_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist TypeLOVForm details" });
        }
        log.info("Created new TypeLOVForm with id: {}", actualEntity.getId());
        return actualEntity.getId();
    }

    @Override
    @Transactional
    public void updateTypeLOV(Long id, TypeLOVForm form) throws TypeLOVException {
        log.info("Updating TypeLOVForm by id: {}", id);

        log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TYPE_LOV_ENTITY_ID.getValue(), id);
        Optional<TypeLOVEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_NO_TYPE_LOV_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TypeLOVException(TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_FOUND_TYPE_LOV_ENTITY_ID.getValue(), id);

        TypeLOVEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("TypeLOVEntity is inactive with id: {}", id);
            throw new TypeLOVException(TypeErrorCode.TYPE_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TypeLOVEntity is active with id: {}", id);

        if(form == null) {
            log.debug("TypeLOVForm is null");
            throw new TypeLOVException(TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of TypeLOVForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("TypeLOVForm has {} errors", err.getErrorCount());
            TypeErrorCode ec = TypeErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TypeLOVForm error detail: {}", ec);
            throw new TypeLOVException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of TypeLOVForm are empty");
            throw new TypeLOVException(TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of TypeLOVForm are valid");

        Optional<TypeLOVEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of TypeLOVForm");
            throw new TypeLOVException(TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from TypeLOVForm to TypeLOVEntity");

        log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_EXISTENCE_BY_NAME.getValue(), form.getName());
        TypeLOVEntity expectedEntity = optExpectedEntity.get();
        if(actualEntity.getName().compareTo(expectedEntity.getName()) == 0 || repository.existsByName(expectedEntity.getName())) {
            log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_EXISTS_BY_NAME.getValue(), expectedEntity.getName());
            throw new TypeLOVException(TypeErrorCode.TYPE_EXISTS,
                    new Object[]{ "name", actualEntity.getName() });
        }
        log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_NON_EXISTENCE_BY_NAME.getValue(), expectedEntity.getName());

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from TypeLOVEntity to TypeLOVForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new TypeLOVException(TypeErrorCode.TYPE_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency type LOV details" });
        }
        log.info("Updated existing TypeLOVEntity with id: {} to version: {}", actualEntity.getId(), actualEntity.getVersion());
    }

    @Override
    @Transactional
    //@Transactional(rollbackFor = TypeModelException.class, propagation = Propagation.REQUIRES_NEW, isolation = Isolation.READ_COMMITTED)
    public void deleteTypeLOV(Long id) throws TypeLOVException {
        log.info("Soft deleting TypeLOVEntity by id: {}", id);

        log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TYPE_LOV_ENTITY_ID.getValue(), id);
        Optional<TypeLOVEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_NO_TYPE_LOV_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TypeLOVException(TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_FOUND_TYPE_LOV_ENTITY_ID.getValue(), id);

        TypeLOVEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("TypeLOVEntity is inactive with id: {}", id);
            throw new TypeLOVException(TypeErrorCode.TYPE_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TypeLOVEntity is active with id: {}", id);

        Set<TypeModelEntity> modelEntities = actualEntity.getModelEntities();
        if(modelEntities != null) {
            log.error("Trying to soft delete {} associated TypeModelEntity with Type LOV id: {}", modelEntities.size(), id);
            for(TypeModelEntity typeModelEntity : modelEntities) {
                Long typeModelId = typeModelEntity.getId();
                try {
                    typeModelService.deleteTypeModel(typeModelId);
                } catch (TypeModelException e) {
                    log.debug("Unable to soft delete TypeModelEntity with id: {}", typeModelId);
                    log.error("Soft deletion failed for TypeModelEntity with Type LOV id: {} because: {}", id, e);
                    // throwing unchecked exception to trigger transaction rollback
                    throw new TOABSystemException(TypeErrorCode.TYPE_ACTION_FAILURE, new Object[] { "deletion", "all associated Type Models"});
                }
            }
            log.error("Successfully soft deleted {} associated TypeModelEntity with Type LOV id: {}", modelEntities.size(), id);
        } else {
            log.error("No TypeModelEntity associated with Type LOV id: {}", id);
        }

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        TypeLOVEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new TypeLOVException(TypeErrorCode.TYPE_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current type LOV details with id:" + id });
        }

        log.info("Soft deleted existing TypeLOVEntity with id: {}", actualEntity.getId());
    }

    @Override
    @Transactional
    public void applyPatchOnTypeLOV(Long id, List<PatchOperationForm> patches) throws TypeLOVException {
        log.info("Patching TypeLOVEntity by id: {}", id);

        log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TYPE_LOV_ENTITY_ID.getValue(), id);
        Optional<TypeLOVEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_NO_TYPE_LOV_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TypeLOVException(TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_FOUND_TYPE_LOV_ENTITY_ID.getValue(), id);

        TypeLOVEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("TypeLOV patch list not provided");
            throw new TypeLOVException(TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("TypeLOV patch list has {} items", patches.size());


        log.debug("Validating patch list items for TypeLOV");
        try {
            toabBaseService.validatePatches(patches, TypeErrorCode.TYPE_EXISTS.getDomain() + ":LOV");
            log.debug("All TypeLOV patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the TypeLOV patch item are invalid");
            throw new TypeLOVException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for TypeLOV");


        log.debug("Patching list items to TypeLOVDto");
        TypeLOVDto patchedTypeLOVForm = new TypeLOVDto();
        try {
            log.debug("Preparing patch list items for TypeLOV");
            JsonNode typeLovDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch typeLovPatch = JsonPatch.fromJson(typeLovDtoTree);
            log.debug("Prepared patch list items for TypeLOV");
            JsonNode blankTypeLOVDtoTree = om.convertValue(new TypeLOVDto(), JsonNode.class);
            JsonNode patchedTypeLOVFormTree = typeLovPatch.apply(blankTypeLOVDtoTree);
            log.debug("Applying patch list items to TypeLOVDto");
            patchedTypeLOVForm = om.treeToValue(patchedTypeLOVFormTree, TypeLOVDto.class);
            log.debug("Applied patch list items to TypeLOVDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to TypeLOVDto: {}", e);
            TypeLOVException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in TypeLOVDto");
                ex = new TypeLOVException(TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new TypeLOVException(TypeErrorCode.TYPE_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to TypeLOVDto: {}", e);
            throw new TypeLOVException(TypeErrorCode.TYPE_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to TypeLOVDto");

        log.debug("Validating patched TypeLOVDto");
        Errors err = new DirectFieldBindingResult(patchedTypeLOVForm, patchedTypeLOVForm.getClass().getSimpleName());
        dtoValidator.validate(patchedTypeLOVForm, err);
        if(err.hasErrors()) {
            log.debug("Patched TypeLOVDto has {} errors", err.getErrorCount());
            TypeErrorCode ec = TypeErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched TypeLOVDto error detail: {}", ec);
            throw new TypeLOVException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched TypeLOVDto are valid");

        log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_EXISTENCE_BY_NAME.getValue(), patchedTypeLOVForm.getName().get());
        if(actualEntity.getName().compareTo(patchedTypeLOVForm.getName().get()) == 0 || repository.existsByName(patchedTypeLOVForm.getName().get())) {
            log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_EXISTS_BY_NAME.getValue(), patchedTypeLOVForm.getName().get());
            throw new TypeLOVException(TypeErrorCode.TYPE_EXISTS,
                    new Object[]{ "name", patchedTypeLOVForm.getName().get() });
        }
        log.debug(TypeLOVMessageTemplate.MSG_TEMPLATE_TYPE_LOV_NON_EXISTENCE_BY_NAME.getValue(), patchedTypeLOVForm.getName().get());


        log.debug("Comparatively copying patched attributes from TypeLOVDto to TypeLOVEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedTypeLOVForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (TypeLOVException) e;
        }
        log.debug("Comparatively copied patched attributes from TypeLOVDto to TypeLOVEntity");

        log.debug("Saving patched TypeLOVEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched TypeLOVEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete TypeLOVEntity with id:{}", id);
            throw new TypeLOVException(TypeErrorCode.TYPE_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency type LOV details with id:" + id });
        }
        log.info("Patched TypeLOVEntity with id:{}", id);
    }
}