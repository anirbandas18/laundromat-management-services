package com.teenthofabud.laundromat.manager.access;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeEntity;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeForm;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeVo;
import org.junit.Assert;
import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import javax.persistence.EntityManager;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@Transactional
@DirtiesContext(classMode = DirtiesContext.ClassMode.BEFORE_EACH_TEST_METHOD)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.ANY)
public class UserTypeIntegrationTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final Long CREATED_BY_USER_ID = 1L;

    private static final String USER_TYPE_URI = "/usertype";
    private static final String USER_TYPE_URI_BY_ID = "/usertype/{id}";
    private static final String USER_TYPE_URI_BY_NAME = "/usertype/name/{name}";

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper om;

    @Autowired
    private EntityManager em;

    private UserTypeForm userTypeForm;
    private UserTypeVo userTypeVo1;
    private UserTypeVo userTypeVo2;
    private UserTypeVo userTypeVo3;
    private UserTypeVo userTypeVo4;
    private UserTypeEntity userTypeEntity1;
    private UserTypeEntity userTypeEntity2;
    private UserTypeEntity userTypeEntity3;
    private UserTypeEntity userTypeEntity4;
    private List<PatchOperationForm> patches;

    @BeforeAll
    private void setUp() {
        userTypeForm = new UserTypeForm();
        userTypeForm.setName("Demo UserType");
        userTypeForm.setDescription("This is for e2e testing of services");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "Sample UserType"),
                new PatchOperationForm("replace", "/active", "true"),
                new PatchOperationForm("replace", "/description", "Patching description attribute of this UserType userType"));

        userTypeVo1 = new UserTypeVo();
        userTypeVo1.setId(1L);
        userTypeVo1.setActive(Boolean.TRUE);
        userTypeVo1.setName("Test UserType 1");
        userTypeVo1.setDescription("This belongs to group 1 for e2e testing");

        userTypeVo2 = new UserTypeVo();
        userTypeVo2.setId(2L);
        userTypeVo2.setActive(Boolean.TRUE);
        userTypeVo2.setName("Test UserType 2");
        userTypeVo2.setDescription("This belongs to group 1 for e2e testing");

        userTypeVo3 = new UserTypeVo();
        userTypeVo3.setId(3L);
        userTypeVo3.setActive(Boolean.TRUE);
        userTypeVo3.setName("Sample UserType 3");
        userTypeVo3.setDescription("This belongs to group 2 for e2e testing");

        userTypeVo4 = new UserTypeVo();
        userTypeVo4.setId(4L);
        userTypeVo4.setActive(Boolean.FALSE);
        userTypeVo4.setName("Sample UserType 4");
        userTypeVo4.setDescription("This belongs to group 2 for e2e testing");

        userTypeEntity1 = new UserTypeEntity();
        userTypeEntity1.setActive(Boolean.TRUE);
        userTypeEntity1.setCreatedBy(CREATED_BY_USER_ID);
        userTypeEntity1.setCreatedOn(LocalDateTime.now());
        userTypeEntity1.setModifiedBy(CREATED_BY_USER_ID);
        userTypeEntity1.setModifiedOn(LocalDateTime.now());
        userTypeEntity1.setVersion(0);
        userTypeEntity1.setName("Test UserType 1");
        userTypeEntity1.setDescription("This belongs to group 1 for e2e testing");

        userTypeEntity2 = new UserTypeEntity();
        userTypeEntity2.setActive(Boolean.TRUE);
        userTypeEntity2.setCreatedBy(CREATED_BY_USER_ID);
        userTypeEntity2.setCreatedOn(LocalDateTime.now());
        userTypeEntity2.setModifiedBy(CREATED_BY_USER_ID);
        userTypeEntity2.setModifiedOn(LocalDateTime.now());
        userTypeEntity2.setVersion(0);
        userTypeEntity2.setName("Test UserType 2");
        userTypeEntity2.setDescription("This belongs to group 1 for e2e testing");


        userTypeEntity3 = new UserTypeEntity();
        userTypeEntity3.setActive(Boolean.TRUE);
        userTypeEntity3.setCreatedBy(CREATED_BY_USER_ID);
        userTypeEntity3.setCreatedOn(LocalDateTime.now());
        userTypeEntity3.setModifiedBy(CREATED_BY_USER_ID);
        userTypeEntity3.setModifiedOn(LocalDateTime.now());
        userTypeEntity3.setVersion(0);
        userTypeEntity3.setName("Sample UserType 3");
        userTypeEntity3.setDescription("This belongs to group 2 for e2e testing");


        userTypeEntity4 = new UserTypeEntity();
        userTypeEntity4.setActive(Boolean.FALSE);
        userTypeEntity4.setCreatedBy(CREATED_BY_USER_ID);
        userTypeEntity4.setCreatedOn(LocalDateTime.now());
        userTypeEntity4.setModifiedBy(CREATED_BY_USER_ID);
        userTypeEntity4.setModifiedOn(LocalDateTime.now());
        userTypeEntity4.setVersion(0);
        userTypeEntity4.setName("Sample UserType 4");
        userTypeEntity4.setDescription("This belongs to group 2 for e2e testing");

        om.registerModule(new Jdk8Module());
        om.registerModule(new JavaTimeModule());

    }

    @BeforeEach
    private void init() {
        em.merge(userTypeEntity1);
        em.merge(userTypeEntity2);
        em.merge(userTypeEntity3);
        em.merge(userTypeEntity4);
    }

    @AfterEach
    private void destroy() {
        em.remove(userTypeEntity1);
        em.remove(userTypeEntity2);
        em.remove(userTypeEntity3);
        em.remove(userTypeEntity4);

        userTypeForm = new UserTypeForm();
        userTypeForm.setName("Demo UserType");
        userTypeForm.setDescription("This is for e2e testing of services");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "Sample UserType"),
                new PatchOperationForm("replace", "/active", "true"),
                new PatchOperationForm("replace", "/description", "Patching description attribute of this UserType userType"));
    }

    @Test
    public void test_UserType_Post_ShouldReturn_201Response_And_NewUserTypeId_WhenPosted_WithValidUserTypeForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(USER_TYPE_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(userTypeForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assert.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_UserType_Post_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        userTypeForm.setName("");

        mvcResult = mockMvc.perform(post(USER_TYPE_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(userTypeForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_UserType_Post_ShouldReturn_409Response_And_ErrorCode_LMS_ACCESS_004_WhenRequested_WithDuplicateUserType() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_EXISTS.getErrorCode();
        String field1Name = "name";
        userTypeForm.setName("Test UserType 1");

        mvcResult = mockMvc.perform(post(USER_TYPE_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(userTypeForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_UserType_Post_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenPosted_WithNoUserTypeForm() throws Exception {
        long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(USER_TYPE_URI)
                .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_UserType_Get_ShouldReturn_200Response_And_UserTypeListNaturallyOrdered_WhenRequested_ForAllUserTypes() throws Exception {
        MvcResult mvcResult = null;
        Set<UserTypeVo> studentList = new TreeSet<>(Arrays.asList(userTypeVo1, userTypeVo2, userTypeVo3, userTypeVo4));
        long expectedUserTypeVoCount = 4;

        mvcResult = this.mockMvc.perform(get(USER_TYPE_URI))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(expectedUserTypeVoCount, om.readValue(mvcResult.getResponse().getContentAsString(), UserTypeVo[].class).length);
    }

    @Test
    public void test_UserType_Get_ShouldReturn_200Response_And_UserTypeDetails_WhenRequested_ById() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(USER_TYPE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(om.writeValueAsString(userTypeVo1), mvcResult.getResponse().getContentAsString());
        Assert.assertEquals(userTypeVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), UserTypeVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_UserType_Get_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(USER_TYPE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_UserType_Get_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_002_WhenRequested_ByAbsentId() throws Exception {
        Long id = 111l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(USER_TYPE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_UserType_Get_ShouldReturn_200Response_And_MatchingUserTypeDetails_WhenRequested_ByName() throws Exception {
        String name = "Test";
        List<UserTypeVo> students = Arrays.asList(userTypeVo1, userTypeVo2);
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(USER_TYPE_URI_BY_NAME, name))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(2, om.readValue(mvcResult.getResponse().getContentAsString(), UserTypeVo[].class).length);
    }

    @Test
    public void test_UserType_Get_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ByEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";

        mvcResult = this.mockMvc.perform(get(USER_TYPE_URI_BY_NAME, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_UserType_Get_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenRequested_ByAbsentName() throws Exception {
        MvcResult mvcResult = null;
        String name = "kk";
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "name";

        mvcResult = this.mockMvc.perform(get(USER_TYPE_URI_BY_NAME, name))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(name));
    }

    @Test
    public void test_UserType_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        Long id = 3l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(USER_TYPE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_UserType_Delete_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001__WhenDeleted_ByEmptyId() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(USER_TYPE_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_UserType_Delete_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenDeleted_ByInvalidId() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(USER_TYPE_URI_BY_ID, "r"))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_UserType_Delete_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_005_WhenDeleted_ByInactiveId() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(USER_TYPE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_UserType_Delete_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenDeleted_ByAbsentId() throws Exception {
        Long id = 111l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(USER_TYPE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_UserType_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndUserTypeDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        userTypeForm.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(USER_TYPE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(userTypeForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_UserType_Put_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenUpdatedBy_EmptyInvalidId_AndUserTypeDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(USER_TYPE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(userTypeForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_UserType_Put_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenUpdated_ByAbsentId_AndUserTypeDetails() throws Exception {
        Long id = 41l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(USER_TYPE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(userTypeForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_UserType_Put_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_005_WhenUpdated_ByInactiveId_AndUserTypeDetails() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(USER_TYPE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(userTypeForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_UserType_Put_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenUpdated_ById_AndNoUserTypeDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(USER_TYPE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_UserType_Put_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ById_AndInvalidName() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        userTypeForm.setName("");

        mvcResult = mockMvc.perform(put(USER_TYPE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(userTypeForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_UserType_Put_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenUpdated_ById_AndEmptyUserTypeDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(USER_TYPE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new UserTypeForm())))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_UserType_Put_ShouldReturn_409Response_And_ErrorCode_LMS_ACCESS_004_WhenUpdated_ById_AndDuplicateUserTypeDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_EXISTS.getErrorCode();
        String field1Name = "name";
        userTypeForm.setName(userTypeEntity1.getName());

        mvcResult = mockMvc.perform(put(USER_TYPE_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(userTypeForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_UserType_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndUserTypeDetails() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(USER_TYPE_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_UserType_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndUserTypeDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(USER_TYPE_URI_BY_ID, " ")
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_UserType_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_003_WhenUpdated_ByInvalidId_AndUserTypeDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(USER_TYPE_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id));
    }

    @Test
    public void test_UserType_Patch_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenUpdated_ByAbsentId_AndUserTypeDetails() throws Exception {
        Long id = 411l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(USER_TYPE_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_UserType_Patch_ShouldReturn_409Response_And_ErrorCode_LMS_ACCESS_002_WhenUpdated_ById_AndDuplicateUserTypeDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_EXISTS.getErrorCode();
        String fieldName = "name";
        String fieldValue = "Sample UserType 3";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", fieldValue));


        mvcResult = this.mockMvc.perform(patch(USER_TYPE_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldValue));
    }

    @Test
    public void test_UserType_Patch_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenUpdated_ById_AndNoUserTypeDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(USER_TYPE_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_UserType_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(USER_TYPE_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_UserType_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(USER_TYPE_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_UserType_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ById_AndInvalidDefinitionOfUserTypeAttribute() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(USER_TYPE_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

}