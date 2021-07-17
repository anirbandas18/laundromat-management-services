package com.teenthofabud.laundromat.manager.access.resource.service.impl;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.fge.jsonpatch.JsonPatch;
import com.github.fge.jsonpatch.JsonPatchException;
import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.core.common.service.TOABBaseService;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.resource.converter.ResourceDto2EntityConverter;
import com.teenthofabud.laundromat.manager.access.resource.converter.ResourceEntity2VoConverter;
import com.teenthofabud.laundromat.manager.access.resource.converter.ResourceForm2EntityConverter;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceDto;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceEntity;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceException;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceForm;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceMessageTemplate;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceVo;
import com.teenthofabud.laundromat.manager.access.resource.mapper.ResourceEntitySelfMapper;
import com.teenthofabud.laundromat.manager.access.resource.mapper.ResourceForm2EntityMapper;
import com.teenthofabud.laundromat.manager.access.resource.repository.ResourceRepository;
import com.teenthofabud.laundromat.manager.access.resource.service.ResourceService;
import com.teenthofabud.laundromat.manager.access.resource.validator.ResourceDtoValidator;
import com.teenthofabud.laundromat.manager.access.resource.validator.ResourceFormRelaxedValidator;
import com.teenthofabud.laundromat.manager.access.resource.validator.ResourceFormValidator;
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
public class ResourceServiceImpl implements ResourceService {

    private static final Comparator<ResourceVo> CMP_BY_NAME = (s1, s2) -> {
        return s1.getName().compareTo(s2.getName());
    };

    private ResourceEntity2VoConverter entity2VoConverter;
    private ResourceForm2EntityConverter form2EntityConverter;
    private ResourceDto2EntityConverter dto2EntityConverter;
    private ResourceForm2EntityMapper form2EntityMapper;
    private ResourceEntitySelfMapper entitySelfMapper;
    private ResourceFormValidator formValidator;
    private ResourceFormRelaxedValidator relaxedFormValidator;
    private ResourceDtoValidator dtoValidator;
    private ResourceRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;

    @Autowired
    public void setEntity2VoConverter(ResourceEntity2VoConverter entity2VoConverter) {
        this.entity2VoConverter = entity2VoConverter;
    }

    @Autowired
    public void setDto2EntityConverter(ResourceDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(ResourceForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(ResourceEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(ResourceFormRelaxedValidator relaxedFormValidator) {
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
    public void setDtoValidator(ResourceDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2EntityConverter(ResourceForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(ResourceRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(ResourceFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private List<ResourceVo> entity2DetailedVoList(List<ResourceEntity> resourceEntityList) {
        List<ResourceVo> resourceDetailsList = new ArrayList<>(resourceEntityList.size());
        for(ResourceEntity entity : resourceEntityList) {
            ResourceVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            resourceDetailsList.add(vo);
        }
        return resourceDetailsList;
    }

    @Override
    @Transactional(readOnly = true)
    public Set<ResourceVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all ResourceEntity by their natural ordering");
        List<ResourceEntity> resourceEntityList = repository.findAll();
        Set<ResourceVo> naturallyOrderedSet = new TreeSet<ResourceVo>(CMP_BY_NAME);
        for(ResourceEntity entity : resourceEntityList) {
            ResourceVo dto = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, dto);
            naturallyOrderedSet.add(dto);
        }
        log.info("{} ResourceVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    @Transactional(readOnly = true)
    public ResourceVo retrieveDetailsById(long id) throws ResourceException {
        log.info("Requesting ResourceEntity by id: {}", id);
        Optional<ResourceEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug("No ResourceEntity found by id: {}", id);
            throw new ResourceException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        ResourceEntity entity = optEntity.get();
        ResourceVo vo = entity2VoConverter.convert(entity);
        log.info("Found ResourceVo by id: {}", id);
        return vo;
    }


    @Override
    @Transactional(readOnly = true)
    public List<ResourceVo> retrieveAllMatchingDetailsByName(String name) throws ResourceException {
        log.info("Requesting ResourceEntity that match with name: {}", name);
        List<ResourceEntity> resourceEntityList = repository.findByNameContaining(name);
        if(resourceEntityList != null && !resourceEntityList.isEmpty()) {
            List<ResourceVo> matchedResourceList = entity2DetailedVoList(resourceEntityList);
            log.info("Found {} ResourceVo matching with name: {}", matchedResourceList.size(),name);
            return matchedResourceList;
        }
        log.debug("No ResourceVo found matching with name: {}", name);
        throw new ResourceException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "name", name });
    }

    @Override
    @Transactional
    public Long createResource(ResourceForm form) throws ResourceException {
        log.info("Creating new ResourceEntity");

        if(form == null) {
            log.debug("ResourceForm provided is null");
            throw new ResourceException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of ResourceForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("ResourceForm has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("ResourceForm error detail: {}", ec);
            throw new ResourceException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of ResourceForm are valid");

        log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_EXISTENCE_BY_NAME.getValue(), form.getName());
        ResourceEntity expectedEntity = form2EntityConverter.convert(form);
        if(repository.existsByName(expectedEntity.getName())) {
            log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_EXISTS_BY_NAME.getValue(), expectedEntity.getName());
            throw new ResourceException(AccessErrorCode.ACCESS_EXISTS,
                    new Object[]{ "name", form.getName() });
        }
        log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_NON_EXISTENCE_BY_NAME.getValue(), expectedEntity.getName());

        log.debug("Saving {}", expectedEntity);
        ResourceEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new ResourceException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist ResourceForm details" });
        }
        log.info("Created new ResourceForm with id: {}", actualEntity.getId());
        return actualEntity.getId();
    }

    @Override
    @Transactional
    public void updateResource(Long id, ResourceForm form) throws ResourceException {
        log.info("Updating ResourceForm by id: {}", id);

        log.debug(ResourceMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_RESOURCE_ENTITY_ID.getValue(), id);
        Optional<ResourceEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(ResourceMessageTemplate.MSG_TEMPLATE_NO_RESOURCE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new ResourceException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(ResourceMessageTemplate.MSG_TEMPLATE_FOUND_RESOURCE_ENTITY_ID.getValue(), id);

        ResourceEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("ResourceEntity is inactive with id: {}", id);
            throw new ResourceException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("ResourceEntity is active with id: {}", id);

        if(form == null) {
            log.debug("ResourceForm is null");
            throw new ResourceException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of ResourceForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("ResourceForm has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("ResourceForm error detail: {}", ec);
            throw new ResourceException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of ResourceForm are empty");
            throw new ResourceException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of ResourceForm are valid");

        Optional<ResourceEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of ResourceForm");
            throw new ResourceException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from ResourceForm to ResourceEntity");

        log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_EXISTENCE_BY_NAME.getValue(), form.getName());
        ResourceEntity expectedEntity = optExpectedEntity.get();
        if(actualEntity.getName().compareTo(expectedEntity.getName()) == 0 || repository.existsByName(expectedEntity.getName())) {
            log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_EXISTS_BY_NAME.getValue(), expectedEntity.getName());
            throw new ResourceException(AccessErrorCode.ACCESS_EXISTS,
                    new Object[]{ "name", actualEntity.getName() });
        }
        log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_NON_EXISTENCE_BY_NAME.getValue(), expectedEntity.getName());

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from ResourceEntity to ResourceForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new ResourceException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency resource details" });
        }
        log.info("Updated existing ResourceEntity with id: {} to version: {}", actualEntity.getId(), actualEntity.getVersion());
    }

    @Override
    @Transactional
    //@Transactional(rollbackFor = AccessModelException.class, propagation = Propagation.REQUIRES_NEW, isolation = Isolation.READ_COMMITTED)
    public void deleteResource(Long id) throws ResourceException {
        log.info("Soft deleting ResourceEntity by id: {}", id);

        log.debug(ResourceMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_RESOURCE_ENTITY_ID.getValue(), id);
        Optional<ResourceEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug(ResourceMessageTemplate.MSG_TEMPLATE_NO_RESOURCE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new ResourceException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(ResourceMessageTemplate.MSG_TEMPLATE_FOUND_RESOURCE_ENTITY_ID.getValue(), id);

        ResourceEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("ResourceEntity is inactive with id: {}", id);
            throw new ResourceException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("ResourceEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        ResourceEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new ResourceException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current resource details with id:" + id });
        }

        log.info("Soft deleted existing ResourceEntity with id: {}", actualEntity.getId());
    }

    @Override
    @Transactional
    public void applyPatchOnResource(Long id, List<PatchOperationForm> patches) throws ResourceException {
        log.info("Patching ResourceEntity by id: {}", id);

        log.debug(ResourceMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_RESOURCE_ENTITY_ID.getValue(), id);
        Optional<ResourceEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(ResourceMessageTemplate.MSG_TEMPLATE_NO_RESOURCE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new ResourceException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(ResourceMessageTemplate.MSG_TEMPLATE_FOUND_RESOURCE_ENTITY_ID.getValue(), id);

        ResourceEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Resource patch list not provided");
            throw new ResourceException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Resource patch list has {} items", patches.size());


        log.debug("Validating patch list items for Resource");
        try {
            toabBaseService.validatePatches(patches, AccessErrorCode.ACCESS_EXISTS.getDomain() + ":LOV");
            log.debug("All Resource patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Resource patch item are invalid");
            throw new ResourceException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Resource");


        log.debug("Patching list items to ResourceDto");
        ResourceDto patchedResourceForm = new ResourceDto();
        try {
            log.debug("Preparing patch list items for Resource");
            JsonNode resourceDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch resourcePatch = JsonPatch.fromJson(resourceDtoTree);
            log.debug("Prepared patch list items for Resource");
            JsonNode blankResourceDtoTree = om.convertValue(new ResourceDto(), JsonNode.class);
            JsonNode patchedResourceFormTree = resourcePatch.apply(blankResourceDtoTree);
            log.debug("Applying patch list items to ResourceDto");
            patchedResourceForm = om.treeToValue(patchedResourceFormTree, ResourceDto.class);
            log.debug("Applied patch list items to ResourceDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to ResourceDto: {}", e);
            ResourceException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in ResourceDto");
                ex = new ResourceException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new ResourceException(AccessErrorCode.ACCESS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to ResourceDto: {}", e);
            throw new ResourceException(AccessErrorCode.ACCESS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to ResourceDto");

        log.debug("Validating patched ResourceDto");
        Errors err = new DirectFieldBindingResult(patchedResourceForm, patchedResourceForm.getClass().getSimpleName());
        dtoValidator.validate(patchedResourceForm, err);
        if(err.hasErrors()) {
            log.debug("Patched ResourceDto has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched ResourceDto error detail: {}", ec);
            throw new ResourceException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched ResourceDto are valid");

        log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_EXISTENCE_BY_NAME.getValue(), patchedResourceForm.getName().get());
        if(actualEntity.getName().compareTo(patchedResourceForm.getName().get()) == 0 || repository.existsByName(patchedResourceForm.getName().get())) {
            log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_EXISTS_BY_NAME.getValue(), patchedResourceForm.getName().get());
            throw new ResourceException(AccessErrorCode.ACCESS_EXISTS,
                    new Object[]{ "name", patchedResourceForm.getName().get() });
        }
        log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_NON_EXISTENCE_BY_NAME.getValue(), patchedResourceForm.getName().get());


        log.debug("Comparatively copying patched attributes from ResourceDto to ResourceEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedResourceForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (ResourceException) e;
        }
        log.debug("Comparatively copied patched attributes from ResourceDto to ResourceEntity");

        log.debug("Saving patched ResourceEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched ResourceEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete ResourceEntity with id:{}", id);
            throw new ResourceException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency resource details with id:" + id });
        }
        log.info("Patched ResourceEntity with id:{}", id);
    }
}