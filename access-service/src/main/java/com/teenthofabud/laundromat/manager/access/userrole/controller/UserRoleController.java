package com.teenthofabud.laundromat.manager.access.userrole.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleException;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleForm;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleMessageTemplate;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleVo;
import com.teenthofabud.laundromat.manager.access.userrole.service.UserRoleService;
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
@RequestMapping("userrole")
@Slf4j
@Tag(name = "UserRole API", description = "Manage UserRoles and their details")
public class UserRoleController {

    private static final String MEDIA_ACCESS_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(UserRoleService service) {
        this.service = service;
    }

    private UserRoleService service;

    @Operation(summary = "Create new UserRole details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created UserRole",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = String.class)) }),
            @ApiResponse(responseCode = "400", description = "UserRole attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "UserRole already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No UserRole attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new UserRole",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Long> postNewUserRole(@RequestBody(required = false) UserRoleForm form) throws UserRoleException {
        log.debug("Requesting to create new userRole");
        if(form != null) {
            Long id = service.createUserRole(form);
            log.debug("Responding with identifier of newly created new userRole");
            return ResponseEntity.status(HttpStatus.CREATED).body(id);
        }
        log.debug("UserRoleForm is null");
        throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update UserRole details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of UserRole",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "UserRole attribute's value is invalid/Role is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No UserRole found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "UserRole already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update UserRole details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> putExistingUserRole(@PathVariable String id, @RequestBody(required = false) UserRoleForm form) throws UserRoleException {
        log.debug("Requesting to update all attributes of existing userRole");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_ID_VALID.getValue(), id);
                if(form != null) {
                    service.updateUserRole(actualId, form);
                    log.debug("Responding with successful updation of attributes for existing userRole");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("UserRoleForm is null");
                throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_ID_INVALID.getValue(), id);
                throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_ID_EMPTY.getValue());
        throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete UserRole by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted UserRole",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "UserRole id is invalid/Role is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No UserRole found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No UserRole attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete UserRole",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public ResponseEntity<Void> deleteExistingUserRole(@PathVariable String id) throws UserRoleException {
        log.debug("Requesting to soft delete userRole");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_ID_VALID.getValue(), id);
                service.deleteUserRole(actualId);
                log.debug("Responding with successful deletion of existing userRole");
                return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
            } catch (NumberFormatException e) {
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_ID_INVALID.getValue(), id);
                throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_ID_EMPTY.getValue());
        throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch UserRole attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of UserRole with the given value",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "UserRole attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No UserRole found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No UserRole attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of UserRole with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_ACCESS_APPLICATION_JSON_PATCH)
    public ResponseEntity<Void> patchExistingUserRole(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws UserRoleException {
        log.debug("Requesting to patch of userRole attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_ID_VALID.getValue(), id);
                if(dtoList != null) {
                    service.applyPatchOnUserRole(actualId, dtoList);
                    log.debug("Responding with successful patch of attributes for existing userRole");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("userRole patch document is null");
                throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_ID_INVALID.getValue(), id);
                throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_ID_EMPTY.getValue());
        throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all UserRole details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available UserRoles and their details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = UserRoleVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<UserRoleVo> getAllUserRoleNaturallyOrdered() {
        log.debug("Requesting all available userRoles by their natural orders");
        Set<UserRoleVo> naturallyOrderedUserRoles = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available userRoles by their natural orders");
        return naturallyOrderedUserRoles;
    }

    @Operation(summary = "Get all UserRole details by userTypeId")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Retrieve all available UserRoles and their details that match the given userTypeId",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = UserRoleVo.class))) }),
        @ApiResponse(responseCode = "400", description = "UserRole userTypeId is invalid",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
        @ApiResponse(responseCode = "404", description = "No UserRoles available with the given userTypeId",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("usertypeid/{userTypeId}")
    public List<UserRoleVo> getAllUserRolesByUserType(@PathVariable String userTypeId) throws UserRoleException {
        log.debug("Requesting all available userRoles with given userTypeId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(userTypeId))) {
            try {
                Long actualUserTypeId = Long.parseLong(userTypeId);
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_ID_VALID.getValue(), actualUserTypeId);
                List<UserRoleVo> matchedByUserTypeId = service.retrieveAllMatchingDetailsByUserType(actualUserTypeId);
                log.debug("Responding with successful retrieval of existing userRole details by userTypeId");
                return matchedByUserTypeId;
            } catch (NumberFormatException e) {
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_ID_INVALID.getValue(), userTypeId);
                throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "userTypeId", userTypeId });
            }
        }
        log.debug("userRole userTypeId is empty");
        throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "userTypeId", userTypeId });
    }

    @Operation(summary = "Get all UserRole details by roleId")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available UserRoles and their details that match the given roleId",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = UserRoleVo.class))) }),
            @ApiResponse(responseCode = "400", description = "UserRole roleId is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No UserRoles available with the given roleId",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("roleid/{roleId}")
    public List<UserRoleVo> getAllUserRolesByRole(@PathVariable String roleId) throws UserRoleException {
        log.debug("Requesting all available userRoles with given roleId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(roleId))) {
            try {
                Long actualRoleId = Long.parseLong(roleId);
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_ID_VALID.getValue(), actualRoleId);
                List<UserRoleVo> matchedByUserTypeId = service.retrieveAllMatchingDetailsByRole(actualRoleId);
                log.debug("Responding with successful retrieval of existing userRole details by roleId");
                return matchedByUserTypeId;
            } catch (NumberFormatException e) {
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_ID_INVALID.getValue(), roleId);
                throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "roleId", roleId });
            }
        }
        log.debug("userRole roleId is empty");
        throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "roleId", roleId });
    }

    @Operation(summary = "Get UserRole details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of UserRole that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = UserRoleVo.class)) }),
            @ApiResponse(responseCode = "400", description = "UserRole id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No UserRole found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public UserRoleVo getUserRoleDetailsById(@PathVariable String id) throws UserRoleException {
        log.debug("Requesting all available userRoles by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_ID_VALID.getValue(), id);
                UserRoleVo userRoleDetails = service.retrieveDetailsById(actualId);
                log.debug("Responding with successful retrieval of existing userRole details by id");
                return userRoleDetails;
            } catch (NumberFormatException e) {
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_ID_INVALID.getValue(), id);
                throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_ID_EMPTY.getValue());
        throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
