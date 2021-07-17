package com.teenthofabud.laundromat.manager.access.resource.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceException;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceForm;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceMessageTemplate;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceVo;
import com.teenthofabud.laundromat.manager.access.resource.service.ResourceService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Set;


@RestController
@RequestMapping("resource")
@Slf4j
@Tag(name = "Resource API", description = "Manage Resources and their details")
public class ResourceController {

    private static final String MEDIA_ACCESS_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(ResourceService service) {
        this.service = service;
    }

    private ResourceService service;

    @Operation(summary = "Create new Resource details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Resource",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = String.class)) }),
            @ApiResponse(responseCode = "400", description = "Resource attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Resource already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Resource attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Resource",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Long> postNewResource(@RequestBody(required = false) ResourceForm form) throws ResourceException {
        log.debug("Requesting to create new resource");
        if(form != null) {
            Long id = service.createResource(form);
            log.debug("Responding with identifier of newly created new resource");
            return ResponseEntity.status(HttpStatus.CREATED).body(id);
        }
        log.debug("ResourceForm is null");
        throw new ResourceException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Resource details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Resource",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Resource attribute's value is invalid/Resource is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Resource found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Resource already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Resource details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> putExistingResource(@PathVariable String id, @RequestBody(required = false) ResourceForm form) throws ResourceException {
        log.debug("Requesting to update all attributes of existing resource");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_ID_VALID.getValue(), id);
                if(form != null) {
                    service.updateResource(actualId, form);
                    log.debug("Responding with successful updation of attributes for existing resource");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("ResourceForm is null");
                throw new ResourceException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_ID_INVALID.getValue(), id);
                throw new ResourceException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_ID_EMPTY.getValue());
        throw new ResourceException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Resource by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Resource",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Resource id is invalid/Resource is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Resource found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Resource attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Resource",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public ResponseEntity<Void> deleteExistingResource(@PathVariable String id) throws ResourceException {
        log.debug("Requesting to soft delete resource");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_ID_VALID.getValue(), id);
                service.deleteResource(actualId);
                log.debug("Responding with successful deletion of existing resource");
                return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
            } catch (NumberFormatException e) {
                log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_ID_INVALID.getValue(), id);
                throw new ResourceException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_ID_EMPTY.getValue());
        throw new ResourceException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Resource attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Resource with the given value",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Resource attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Resource found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Resource attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Resource with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_ACCESS_APPLICATION_JSON_PATCH)
    public ResponseEntity<Void> patchExistingResource(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws ResourceException {
        log.debug("Requesting to patch of resource attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_ID_VALID.getValue(), id);
                if(dtoList != null) {
                    service.applyPatchOnResource(actualId, dtoList);
                    log.debug("Responding with successful patch of attributes for existing resource");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("resource patch document is null");
                throw new ResourceException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_ID_INVALID.getValue(), id);
                throw new ResourceException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_ID_EMPTY.getValue());
        throw new ResourceException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Resource details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Resources and their details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = ResourceVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<ResourceVo> getAllResourceNaturallyOrdered() {
        log.debug("Requesting all available resources by their natural orders");
        Set<ResourceVo> naturallyOrderedResources = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available resources by their natural orders");
        return naturallyOrderedResources;
    }

    @Operation(summary = "Get all Resource details by name")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Retrieve all available Resources and their details that match the given name",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = ResourceVo.class))) }),
        @ApiResponse(responseCode = "400", description = "Resource name is invalid",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
        @ApiResponse(responseCode = "404", description = "No Resources available with the given name",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("name/{name}")
    public List<ResourceVo> getAllResourcesByName(@PathVariable String name) throws ResourceException {
        log.debug("Requesting all available resources with given name");
        if(StringUtils.hasText(StringUtils.trimWhitespace(name))) {
            List<ResourceVo> matchedByNames = service.retrieveAllMatchingDetailsByName(name);
            log.debug("Responding with all available resources with given name");
            return matchedByNames;
        }
        log.debug("resource name is empty");
        throw new ResourceException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "name", name });
    }

    @Operation(summary = "Get Resource details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Resource that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ResourceVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Resource id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Resource found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public ResourceVo getResourceDetailsById(@PathVariable String id) throws ResourceException {
        log.debug("Requesting all available resources by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_ID_VALID.getValue(), id);
                ResourceVo resourceDetails = service.retrieveDetailsById(actualId);
                log.debug("Responding with successful retrieval of existing resource details by id");
                return resourceDetails;
            } catch (NumberFormatException e) {
                log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_ID_INVALID.getValue(), id);
                throw new ResourceException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(ResourceMessageTemplate.MSG_TEMPLATE_RESOURCE_ID_EMPTY.getValue());
        throw new ResourceException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
