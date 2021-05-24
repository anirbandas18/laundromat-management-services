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
import com.teenthofabud.laundromat.manager.type.converter.entity2vo.TypeLOVEntity2VoConverter;
import com.teenthofabud.laundromat.manager.type.converter.form2entity.TypeLOVForm2EntityConverter;
import com.teenthofabud.laundromat.manager.type.model.constants.LOVType;
import com.teenthofabud.laundromat.manager.type.model.dto.TypeLOVDto;
import com.teenthofabud.laundromat.manager.type.model.entity.TypeLOVEntity;
import com.teenthofabud.laundromat.manager.type.model.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.type.model.error.TypeException;
import com.teenthofabud.laundromat.manager.type.model.form.TypeLOVForm;
import com.teenthofabud.laundromat.manager.type.model.vo.TypeLOVVo;
import com.teenthofabud.laundromat.manager.type.repository.TypeLOVRepository;
import com.teenthofabud.laundromat.manager.type.service.TypeLOVService;
import com.teenthofabud.laundromat.manager.type.validator.dto.TypeLOVDtoValidator;
import com.teenthofabud.laundromat.manager.type.validator.form.TypeLOVFormValidator;
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
public class TypeLOVServiceImpl implements TypeLOVService {

    private static final Comparator<TypeLOVVo> CMP_BY_NAME = (s1, s2) -> {
        return s1.getName().compareTo(s2.getName());
    };

    private static RelaxedValidator<TypeLOVForm> LOOSE_CURRENCY_TYPE_VALIDATOR;
    private static ComparativePatchConverter<TypeLOVEntity, TypeLOVDto> COMPARE_AND_COPY_PATCH;
    private static ComparativeFormConverter<TypeLOVEntity, TypeLOVEntity> COMPARE_AND_COPY_ENTITY;
    private static ComparativeFormConverter<TypeLOVEntity, TypeLOVForm> COMPARE_AND_COPY_FORM;

    private TypeLOVEntity2VoConverter entity2VoConverter;
    private TypeLOVForm2EntityConverter form2EntityConverter;
    private TypeLOVFormValidator formValidator;
    private TypeLOVDtoValidator dtoValidator;
    private TypeLOVRepository repository;
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
    public void setDtoValidator(TypeLOVDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setEntity2VOConverter(TypeLOVEntity2VoConverter entity2VoConverter) {
        this.entity2VoConverter = entity2VoConverter;
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

    @Override
    @PostConstruct
    public void init() {
        LOOSE_CURRENCY_TYPE_VALIDATOR = (form, errors) -> {
            if(form.getName() != null && form.getName().length() == 0) {
                errors.rejectValue("name", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
                log.debug("TypeLOVForm.name is empty");
                return false;
            }
            log.debug("TypeLOVForm.name is valid");
            return true;
        };

        COMPARE_AND_COPY_PATCH = (actualEntity, form) -> {
            boolean changeSW = false;
            if(form.getDescription().isPresent()) {
                actualEntity.setDescription(form.getDescription().get());
                changeSW = true;
                log.debug("TypeLOVDto.description is valid");
            }
            if(form.getName().isPresent()) {
                actualEntity.setName(form.getName().get());
                changeSW = true;
                log.debug("TypeLOVDto.name is valid");
            }
            if(form.getActive().isPresent()) {
                actualEntity.setActive(Boolean.valueOf(form.getActive().get()));
                changeSW = true;
                log.debug("TypeLOVDto.active is valid");
            }
            if(changeSW) {
                log.debug("All provided TypeLOVDto attributes are valid");
                actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
                return;
            }
            log.debug("Not all provided TypeLOVDto attributes are valid");
        };

        COMPARE_AND_COPY_ENTITY = (source, target) -> {
            TypeLOVEntity expectedEntity = new TypeLOVEntity();
            boolean changeSW = false;
            if(source.getId() != null && source.getId() > 0 && source.getId().compareTo(target.getId()) != 0) {
                target.setId(source.getId());
                changeSW = true;
                log.debug("Source TypeLOVEntity.id is valid");
            }
            if(source.getDescription() != null && source.getDescription().compareTo(target.getDescription()) != 0) {
                target.setDescription(source.getDescription());
                changeSW = true;
                log.debug("Source TypeLOVEntity.description is valid");
            }
            if(source.getName() != null && StringUtils.hasText(source.getName()) && source.getName().compareTo(target.getName()) != 0) {
                target.setName(source.getName());
                changeSW = true;
                log.debug("Source TypeLOVEntity.name is valid");
            }
            if(changeSW) {
                log.debug("All provided TypeLOVEntity attributes are valid");
                return Optional.of(target);
            } else {
                log.debug("Not all provided TypeLOVEntity attributes are valid");
                return Optional.empty();
            }
        };

        COMPARE_AND_COPY_FORM = (actualEntity, form) -> {
            TypeLOVEntity expectedEntity = new TypeLOVEntity();
            boolean changeSW = false;
            // direct copy
            expectedEntity.setId(actualEntity.getId());
            log.debug("Directly copying TypeLOVEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
            expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
            log.debug("Directly copying TypeLOVEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
            expectedEntity.setActive(actualEntity.getActive());
            log.debug("Directly copying TypeLOVEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
            // comparative copy
            if(StringUtils.hasText(form.getName()) && form.getName().compareTo(actualEntity.getName()) != 0) {
                expectedEntity.setName(form.getName());
                changeSW = true;
                log.debug("TypeLOVForm.name: {} is same as TypeLOVEntity.name: {}", form.getName(), actualEntity.getName());
            } else {
                expectedEntity.setName(actualEntity.getName());
                changeSW = true;
                log.debug("TypeLOVForm.name: {} is different to TypeLOVEntity.name: {}", form.getName(), actualEntity.getName());
            }
            if(StringUtils.hasText(form.getDescription()) &&
                    form.getDescription().toLowerCase().compareTo(actualEntity.getDescription().toLowerCase()) != 0) {
                expectedEntity.setDescription(form.getDescription());
                changeSW = true;
                log.debug("TypeLOVForm.description: {} is same as TypeLOVEntity.description: {}", form.getDescription(), actualEntity.getDescription());
            } else {
                expectedEntity.setDescription(actualEntity.getDescription());
                changeSW = true;
                log.debug("TypeLOVForm.description: {} is different to TypeLOVEntity.description: {}", form.getDescription(), actualEntity.getDescription());
            }
            return changeSW ? Optional.of(expectedEntity) : Optional.empty();
        };
        log.debug("Initialized bean with pre-requisites");
    }

    private List<TypeLOVVo> entity2DetailedVoList(List<TypeLOVEntity> studentEntityList) {
        List<TypeLOVVo> studentDetailsList = new ArrayList<>(studentEntityList.size());
        for(TypeLOVEntity entity : studentEntityList) {
            TypeLOVVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            studentDetailsList.add(vo);
        }
        return studentDetailsList;
    }

    @Override
    @Transactional(readOnly = true)
    public Set<TypeLOVVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all TypeLOVEntity by their natural ordering");
        List<TypeLOVEntity> studentEntityList = repository.findAll();
        Set<TypeLOVVo> naturallyOrderedSet = new TreeSet<TypeLOVVo>();
        for(TypeLOVEntity entity : studentEntityList) {
            TypeLOVVo dto = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, dto);
            naturallyOrderedSet.add(dto);
        }
        log.info("{} TypeLOVVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    @Transactional(readOnly = true)
    public TypeLOVVo retrieveDetailsById(long id) throws TypeException {
        log.info("Requesting TypeLOVEntity by id: {}", id);
        Optional<TypeLOVEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug("No TypeLOVEntity found by id: {}", id);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        TypeLOVEntity entity = optEntity.get();
        if(!entity.getActive()) {
            log.debug("TypeLOVEntity is inactive by id: {}", id);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_INACTIVE, new Object[] { String.valueOf(id) });
        }
        TypeLOVVo vo = entity2VoConverter.convert(entity);
        log.info("Found TypeLOVVo by id: {}", id);
        return vo;
    }


    @Override
    @Transactional(readOnly = true)
    public List<TypeLOVVo> retrieveAllMatchingDetailsByName(String name) throws TypeException {
        log.info("Requesting TypeLOVEntity that match with name: {}", name);
        List<TypeLOVEntity> studentEntityList = repository.findByNameContaining(name);
        if(studentEntityList != null && !studentEntityList.isEmpty()) {
            List<TypeLOVVo> matchedTypeLOVList = entity2DetailedVoList(studentEntityList);
            log.info("Found {} TypeLOVVo matching with name: {}", matchedTypeLOVList.size(),name);
            return matchedTypeLOVList;
        }
        log.debug("No TypeLOVVo found matching with name: {}", name);
        throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "name", name });
    }

    @Override
    @Transactional
    public Long createTypeLOV(TypeLOVForm form) throws TypeException {
        log.info("Creating new TypeLOVEntity");

        if(form == null) {
            log.debug("TypeLOVForm provided is null");
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "not provided" });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of TypeLOVForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("TypeLOVForm has {} errors", err.getErrorCount());
            TypeErrorCode ec = TypeErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TypeLOVForm error detail: {}", ec);
            throw new TypeException(LOVType.CURRENCY_TYPE, ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getDefaultMessage() });
        }
        log.debug("All attributes of TypeLOVForm are valid");

        log.debug("Checking existence of TypeLOVEntity with name: {}", form.getName());
        TypeLOVEntity expectedEntity = form2EntityConverter.convert(form);
        if(repository.existsByName(expectedEntity.getName())) {
            log.debug("TypeLOVEntity already exists with name: {}", expectedEntity.getName());
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_EXISTS,
                    new Object[]{ "name", form.getName() });
        }
        log.debug("No TypeLOVEntity exists with name: {}", expectedEntity.getName());

        log.debug("Saving {}", expectedEntity);
        TypeLOVEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist TypeLOVForm details" });
        }
        log.info("Created new TypeLOVForm with id: {}", actualEntity.getId());
        return actualEntity.getId();
    }

    @Override
    @Transactional
    public void updateTypeLOV(Long id, TypeLOVForm form) throws TypeException {
        log.info("Updating TypeLOVForm by id: {}", id);

        log.debug("Searching for TypeLOVEntity with id: {}", id);
        Optional<TypeLOVEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug("No TypeLOVEntity available with id: {}", id);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug("Found TypeLOVEntity with id: {}", id);

        TypeLOVEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("TypeLOVEntity is inactive with id: {}", id);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TypeLOVEntity is active with id: {}", id);

        if(form == null) {
            log.debug("TypeLOVForm is null");
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "not provided" });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of TypeLOVForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = LOOSE_CURRENCY_TYPE_VALIDATOR.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("TypeLOVForm has {} errors", err.getErrorCount());
            TypeErrorCode ec = TypeErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TypeLOVForm error detail: {}", ec);
            throw new TypeException(LOVType.CURRENCY_TYPE, ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getDefaultMessage() });
        } else if (!allEmpty) {
            log.debug("All attributes of TypeLOVForm are empty");
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of TypeLOVForm are valid");

        Optional<TypeLOVEntity> optExpectedEntity = COMPARE_AND_COPY_FORM.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("All attributes of TypeLOVForm are empty");
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("Successfully compared and copied attributes from TypeLOVForm to TypeLOVEntity");

        log.debug("Checking existence of TypeLOVEntity with name: {}", form.getName());
        TypeLOVEntity expectedEntity = optExpectedEntity.get();
        if(repository.existsByName(expectedEntity.getName())) {
            log.debug("TypeLOVEntity already exists with name: {}", expectedEntity.getName());
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_EXISTS,
                    new Object[]{ "name", actualEntity.getName() });
        }
        log.debug("No TypeLOVEntity exists with name: {}", expectedEntity.getName());

        COMPARE_AND_COPY_ENTITY.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from TypeLOVEntity to TypeLOVForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency type LOV details" });
        }
        log.info("Updated existing TypeLOVEntity with id: {} to version: {}", actualEntity.getId(), actualEntity.getVersion());
    }

    @Override
    @Transactional
    public void deleteTypeLOV(Long id) throws TypeException {
        log.info("Soft deleting TypeLOVEntity by id: {}", id);

        log.debug("Searching for TypeLOVEntity with id: {}", id);
        Optional<TypeLOVEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug("No TypeLOVEntity available with id: {}", id);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug("Found TypeLOVEntity with id: {}", id);

        TypeLOVEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("TypeLOVEntity is inactive with id: {}", id);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TypeLOVEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        TypeLOVEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current type LOV details with id:" + id });
        }

        log.info("Soft deleted existing TypeLOVEntity with id: {}", actualEntity.getId());
    }

    @Override
    @Transactional
    public void applyPatchOnTypeLOV(Long id, List<PatchOperationForm> patches) throws TypeException {
        log.info("Patching TypeLOVEntity by id: {}", id);

        log.debug("Searching for TypeLOVEntity with id: {}", id);
        Optional<TypeLOVEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug("No TypeLOVEntity available with id: {}", id);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug("Found TypeLOVEntity with id: {}", id);

        TypeLOVEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("TypeLOV patch list not provided");
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", "not provided" });
        }
        log.debug("TypeLOV patch list has {} items", patches.size());


        log.debug("Validating patch list items for TypeLOV");
        try {
            toabBaseService.validatePatches(patches, TypeErrorCode.TYPE_EXISTS.getDomain() + ":" + LOVType.CURRENCY_TYPE.name());
            log.debug("All TypeLOV patch list items are valid");
        } catch (TOABBaseException e) {
            log.debug("Some of the TypeLOV patch item are invalid");
            throw new TypeException(LOVType.CURRENCY_TYPE, e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for TypeLOV");


        log.debug("Patching list items to TypeLOVDto");
        TypeLOVDto patchedTypeLOVForm = new TypeLOVDto();
        try {
            log.debug("Preparing patch list items for TypeLOV");
            JsonNode studentDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch studentPatch = JsonPatch.fromJson(studentDtoTree);
            log.debug("Prepared patch list items for TypeLOV");
            JsonNode blankTypeLOVDtoTree = om.convertValue(new TypeLOVDto(), JsonNode.class);
            JsonNode patchedTypeLOVFormTree = studentPatch.apply(blankTypeLOVDtoTree);
            log.debug("Applying patch list items to TypeLOVDto");
            patchedTypeLOVForm = om.treeToValue(patchedTypeLOVFormTree, TypeLOVDto.class);
            log.debug("Applied patch list items to TypeLOVDto");
        } catch (IOException | JsonPatchException e) {
            log.debug("Failed to patch list items to TypeLOVDto: {}", e);
            e.printStackTrace();
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to TypeLOVDto");

        log.debug("Validating patched TypeLOVForm");
        Errors err = new DirectFieldBindingResult(patchedTypeLOVForm, patchedTypeLOVForm.getClass().getSimpleName());
        dtoValidator.validate(patchedTypeLOVForm, err);
        if(err.hasErrors()) {
            log.debug("Patched TypeLOVForm has {} errors", err.getErrorCount());
            TypeErrorCode ec = TypeErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched TypeLOVForm error detail: {}", ec);
            throw new TypeException(LOVType.CURRENCY_TYPE, ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getDefaultMessage() });
        }
        log.debug("All attributes of patched TypeLOVForm are valid");

        log.debug("Comparatively copying patched attributes from TypeLOVDto to TypeLOVEntity");
        COMPARE_AND_COPY_PATCH.compareAndMap(actualEntity, patchedTypeLOVForm);
        log.debug("Comparatively copied patched attributes from TypeLOVDto to TypeLOVEntity");

        log.debug("Saving patched TypeLOVEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched TypeLOVEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete TypeLOVEntity with id:{}", id);
            throw new TypeException(LOVType.CURRENCY_TYPE, TypeErrorCode.TYPE_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency type LOV details with id:" + id });
        }
        log.info("Patched TypeLOVEntity with id:{}", id);
    }
}