package com.teenthofabud.laundromat.manager.access.permission.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionException;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionForm;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionMessageTemplate;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionVo;
import com.teenthofabud.laundromat.manager.access.permission.service.PermissionService;
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
@RequestMapping("permission")
@Slf4j
@Tag(name = "Permission API", description = "Manage Permissions and their details")
public class PermissionController {

    private static final String MEDIA_ACCESS_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(PermissionService service) {
        this.service = service;
    }

    private PermissionService service;

    @Operation(summary = "Create new Permission details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Permission",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = String.class)) }),
            @ApiResponse(responseCode = "400", description = "Permission attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Permission already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Permission attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Permission",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Long> postNewPermission(@RequestBody(required = false) PermissionForm form) throws PermissionException {
        log.debug("Requesting to create new permission");
        if(form != null) {
            Long id = service.createPermission(form);
            log.debug("Responding with identifier of newly created new permission");
            return ResponseEntity.status(HttpStatus.CREATED).body(id);
        }
        log.debug("PermissionForm is null");
        throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Permission details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Permission",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Permission attribute's value is invalid/Operation is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Permission found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Permission already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Permission details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> putExistingPermission(@PathVariable String id, @RequestBody(required = false) PermissionForm form) throws PermissionException {
        log.debug("Requesting to update all attributes of existing permission");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_ID_VALID.getValue(), id);
                if(form != null) {
                    service.updatePermission(actualId, form);
                    log.debug("Responding with successful updation of attributes for existing permission");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("PermissionForm is null");
                throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_ID_INVALID.getValue(), id);
                throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_ID_EMPTY.getValue());
        throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Permission by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Permission",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Permission id is invalid/Operation is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Permission found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Permission attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Permission",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public ResponseEntity<Void> deleteExistingPermission(@PathVariable String id) throws PermissionException {
        log.debug("Requesting to soft delete permission");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_ID_VALID.getValue(), id);
                service.deletePermission(actualId);
                log.debug("Responding with successful deletion of existing permission");
                return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
            } catch (NumberFormatException e) {
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_ID_INVALID.getValue(), id);
                throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_ID_EMPTY.getValue());
        throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Permission attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Permission with the given value",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Permission attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Permission found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Permission attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Permission with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_ACCESS_APPLICATION_JSON_PATCH)
    public ResponseEntity<Void> patchExistingPermission(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws PermissionException {
        log.debug("Requesting to patch of permission attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_ID_VALID.getValue(), id);
                if(dtoList != null) {
                    service.applyPatchOnPermission(actualId, dtoList);
                    log.debug("Responding with successful patch of attributes for existing permission");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("permission patch document is null");
                throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_ID_INVALID.getValue(), id);
                throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_ID_EMPTY.getValue());
        throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Permission details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Permissions and their details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = PermissionVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<PermissionVo> getAllPermissionNaturallyOrdered() {
        log.debug("Requesting all available permissions by their natural orders");
        Set<PermissionVo> naturallyOrderedPermissions = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available permissions by their natural orders");
        return naturallyOrderedPermissions;
    }

    @Operation(summary = "Get all Permission details by resourceId")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Retrieve all available Permissions and their details that match the given resourceId",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = PermissionVo.class))) }),
        @ApiResponse(responseCode = "400", description = "Permission resourceId is invalid",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
        @ApiResponse(responseCode = "404", description = "No Permissions available with the given resourceId",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("resourceId/{resourceId}")
    public List<PermissionVo> getAllPermissionsByResource(@PathVariable String resourceId) throws PermissionException {
        log.debug("Requesting all available permissions with given resourceId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(resourceId))) {
            try {
                Long actualResourceId = Long.parseLong(resourceId);
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_ID_VALID.getValue(), actualResourceId);
                List<PermissionVo> matchedByResourceId = service.retrieveAllMatchingDetailsByResource(actualResourceId);
                log.debug("Responding with successful retrieval of existing permission details by resourceId");
                return matchedByResourceId;
            } catch (NumberFormatException e) {
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_ID_INVALID.getValue(), resourceId);
                throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "resourceId", resourceId });
            }
        }
        log.debug("permission resourceId is empty");
        throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "resourceId", resourceId });
    }

    @Operation(summary = "Get all Permission details by operationId")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Permissions and their details that match the given operationId",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = PermissionVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Permission operationId is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Permissions available with the given operationId",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("operationId/{operationId}")
    public List<PermissionVo> getAllPermissionsByOperation(@PathVariable String operationId) throws PermissionException {
        log.debug("Requesting all available permissions with given operationId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(operationId))) {
            try {
                Long actualOperationId = Long.parseLong(operationId);
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_ID_VALID.getValue(), actualOperationId);
                List<PermissionVo> matchedByResourceId = service.retrieveAllMatchingDetailsByOperation(actualOperationId);
                log.debug("Responding with successful retrieval of existing permission details by operationId");
                return matchedByResourceId;
            } catch (NumberFormatException e) {
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_ID_INVALID.getValue(), operationId);
                throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "operationId", operationId });
            }
        }
        log.debug("permission operationId is empty");
        throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "operationId", operationId });
    }

    @Operation(summary = "Get Permission details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Permission that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = PermissionVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Permission id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Permission found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public PermissionVo getPermissionDetailsById(@PathVariable String id) throws PermissionException {
        log.debug("Requesting all available permissions by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_ID_VALID.getValue(), id);
                PermissionVo permissionDetails = service.retrieveDetailsById(actualId);
                log.debug("Responding with successful retrieval of existing permission details by id");
                return permissionDetails;
            } catch (NumberFormatException e) {
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_ID_INVALID.getValue(), id);
                throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_ID_EMPTY.getValue());
        throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
