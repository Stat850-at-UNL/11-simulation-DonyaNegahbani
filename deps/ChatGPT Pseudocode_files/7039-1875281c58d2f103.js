"use strict";(self.webpackChunk_N_E=self.webpackChunk_N_E||[]).push([[7039],{37039:function(e,a,i){i.d(a,{Z:function(){return W}});var n,t,r=i(21722),s=i(96237),l=i(39324),o=i(71209),d=i(10064),m=i(64502),c=i(38104),u=i(35250),f=i(5053),g=i(86646),p=i(17944),h=i(82473),v=i(25494),b=i(18939),x=i.n(b),M=i(70079),w=i(1454),j=i(70671),I=i(32004),N=i(48133),_=i(13205),C=i(62470),S=i(40558),k=i(19841),y=i(94968),Z=i(15420),T=i(37394),A=i(15635),R=/^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/;function P(e){return e.split(/[\s,]+/)}function V(e){var a=e.value,i=e.onChange,n=e.className,t=(0,j.Z)(),r=(0,d._)((0,M.useState)(""),2),s=r[0],l=r[1],o=(0,M.useCallback)(function(e){if(s)switch(e.key){case"Enter":case",":case" ":var n=P(s);i((0,m._)(a).concat((0,m._)(n.map(B)))),l(""),e.preventDefault()}},[s,i,a]);return(0,u.jsx)(Z.ZP,{className:(0,k.default)("react-select-container",n),isMulti:!0,onBlur:function(e){if(s){var n=P(s);i((0,m._)(a).concat((0,m._)(n.map(B)))),l(s),e.preventDefault()}},classNamePrefix:"react-select",components:E,inputValue:s,isClearable:!0,menuIsOpen:!1,onChange:i,onInputChange:l,onKeyDown:o,placeholder:t.formatMessage(L.placeholder),value:a,"aria-labelledby":"add-emails-label",name:"add-member-emails"})}var B=function(e){return{label:e,value:e,isValid:R.test(e)}},E={DropdownIndicator:function(){return null},IndicatorsContainer:function(){return null},ClearIndicator:function(){return null},MultiValueRemove:function(){return null},MultiValue:function(e){var a=(0,j.Z)(),i=(0,u.jsxs)("span",{className:"flex items-center gap-1",children:[(0,u.jsx)("span",{children:e.children}),(0,u.jsx)(w.q5L,{className:"icon-sm"})]}),n=e.data.isValid?i:(0,u.jsx)(A.u,{label:a.formatMessage(L.tooltipInvalidEmail,{email:e.data.value}),side:"top",children:i});return(0,u.jsx)(T.c.MultiValue,(0,o._)((0,l._)({},e),{children:(0,u.jsx)("button",{className:(0,k.default)("m-1 cursor-pointer rounded-full border border-transparent py-1 pl-3 pr-2 text-sm dark:text-gray-300",e.data.isValid?"bg-gray-50 hover:border-gray-300 hover:bg-gray-100 dark:bg-gray-800 dark:hover:border-gray-600 dark:hover:bg-gray-700":"bg-red-200 text-red-500 dark:text-red-500"),onClick:function(a){var i,n;null===(n=(i=e.removeProps).onClick)||void 0===n||n.call(i,a),a.preventDefault()},"aria-label":a.formatMessage(L.removeMember,{member:e.data.value}),children:n})}))}},L=(0,y.vU)({membersAdded:{id:"emailsTextarea.membersAdded",defaultMessage:"+{count} {count, plural, one {member} other {members}}",description:"Current number of members that will be added to the workspace"},removeMember:{id:"emailsTextarea.removeMember",defaultMessage:"Remove {member}",description:"Remove a member from the list of members to be added"},tooltipInvalidEmail:{id:"emailsTextarea.tooltipInvalidEmail",defaultMessage:'"{email}" may not be a valid email',description:"Tooltip for invalid email addresses"},clearAllEntries:{id:"emailsTextarea.clearAllEntries",defaultMessage:"Clear all",description:"Clear all entries in the list of members to be added"},placeholder:{id:"emailsTextarea.placeholder",defaultMessage:"Type an email and press enter...",description:"Placeholder for the insert emails textarea"}}),F=i(89319),z=i(7171),D=i(72290),H=i(12296);function W(e){var a,i,n,b,k,y,Z=e.workspace,T=e.canResendInviteEmails,A=e.onCancel,P=e.cancelButtonText,B=e.onSuccess,E=e.fullWidthButtons,L=void 0!==E&&E,W=(0,H.Gk)(Z.id),U=(0,d._)((0,M.useState)(!1),2),q=U[0],K=U[1],$=(0,d._)((0,M.useState)(!1),2),G=$[0],Q=$[1],Y=(0,M.useRef)(null),X=(0,p.ec)(p.F_.isTeamPlan),ee=(0,j.Z)(),ea=(0,d._)((0,M.useState)(!0),2),ei=ea[0],en=ea[1],et=(0,d._)((0,M.useState)(!1),2),er=et[0],es=et[1],el=(0,d._)((0,M.useState)(t.INITIAL),2),eo=el[0],ed=el[1],em=(0,d._)((0,M.useState)([]),2),ec=em[0],eu=em[1],ef=(0,d._)((0,M.useState)([]),2),eg=ef[0],ep=ef[1],eh=(a=Z.id,n=(0,j.Z)(),b=(0,h.NL)(),(0,v.D)({mutationFn:(i=(0,r._)(function(e){var i,t,s,l,o,d;return(0,c.Jh)(this,function(m){switch(m.label){case 0:var u;for(i=e.emails,t=e.role,s=e.resendEmails,l=[],o=Math.max(5,Math.round(i.length/10)),d=0;d<i.length;d+=o)l.push(i.slice(d,d+o));return u=(0,r._)(function(e){return(0,c.Jh)(this,function(i){return[2,g.ZP.addWorkspaceUsers(a,e,t,s).then(function(e){return e.errored_emails.length>0&&D.m.danger(n.formatMessage(J.inviteMessageFailure,{emailsStr:e.errored_emails.map(function(e){var a=e.email_address,i=e.error;return"".concat(a,": ").concat(i)}).join(", ")})),e.account_invites}).catch(function(e){throw D.m.danger(e.message),e})]})}),[4,Promise.all(l.map(function(e){return function(e){return u.apply(this,arguments)}(e)}))];case 1:return[2,m.sent().flat()]}})}),function(e){return i.apply(this,arguments)}),onSuccess:function(){b.invalidateQueries({queryKey:["workspace","invites",a]}),null==B||B()}})).mutateAsync,ev=O(W),eb=(k=(0,r._)(function(e){var a,i,n;return(0,c.Jh)(this,function(r){switch(r.label){case 0:if(!(i=null===(a=e.target.files)||void 0===a?void 0:a[0]))return[3,2];return[4,new Promise(function(e){if(i.size/1048576>25)e({status:"error",reason:"file_too_large"});else if("text/csv"!==i.type)e({status:"error",reason:"invalid_file_type"});else{var a=new FileReader;a.readAsText(i),a.onload=function(a){var i,n=G,t=(null===(i=a.target)||void 0===i?void 0:i.result).split("\n"),r=new Set;e({status:"success",newRows:t.reduce(function(e,a){var i,t=(0,d._)(a.split(",").map(function(e){return e.trim()}),2),s=t[0],l=t[1];return R.test(s)&&!r.has(s)&&(void 0!==ev[l]||n||(n=!0),e.push({email:s,role:null!==(i=ev[l])&&void 0!==i?i:ev.member})),r.add(s),e},[]),hasWarning:n})}}})];case 1:"success"===(n=r.sent()).status?(eu(n.newRows),ed(t.CSV_TABLE),Q(n.hasWarning)):"file_too_large"===n.reason?D.m.danger(ee.formatMessage(J.fileTooLargeWarning,{maxSize:"".concat(25,"MB")}),{duration:5}):"invalid_file_type"===n.reason&&D.m.danger(ee.formatMessage(J.fileIncorrectTypeWarning),{duration:5}),r.label=2;case 2:return[2]}})}),function(e){return k.apply(this,arguments)}),ex=function(e,a,i){var n=(0,m._)(ec);n[e]=(0,o._)((0,l._)({},n[e]),(0,s._)({},a,i)),eu(n)},eM=function(e){eu((0,m._)(ec.slice(0,e)).concat((0,m._)(ec.slice(e+1))))},ew=(0,M.useCallback)(function(){eu([]),ed(t.INITIAL),es(!1),ep([])},[]),ej=(y=(0,r._)(function(){var e,a;return(0,c.Jh)(this,function(i){switch(i.label){case 0:es(!0),i.label=1;case 1:return i.trys.push([1,,3,4]),ec.length>5&&D.m.info(ee.formatMessage(J.inviteMessageLoading,{numRows:ec.length}),{hasCloseButton:!0}),e=[["admin",f.r3.ADMIN],["owner",f.r3.OWNER],["member",f.r3.STANDARD]],[4,Promise.all(x()(e.map(function(e){var a=(0,d._)(e,2),i=a[0],n=a[1],t=ec.filter(function(e){return e.role===i}).map(function(e){return e.email});return t.length>0&&eh({emails:t,role:n,resendEmails:ei})})))];case 2:return(a=i.sent().flat().length)>0&&D.m.success(ee.formatMessage(J.inviteSuccessToast,{numRows:a,userStr:1===a?"user":"users",workspaceName:Z.name})),null==B||B(),ew(),[3,4];case 3:return es(!1),[7];case 4:return[2]}})}),function(){return y.apply(this,arguments)}),eI=(0,u.jsx)(N.z,{color:"primary",size:L?"large":"medium",disabled:er||0===ec.length,onClick:ej,children:er?ee.formatMessage(J.inviteMemberInviteSubmitButtonLoading):ee.formatMessage(J.inviteMemberInviteSubmitButton)}),eN=(0,u.jsx)(N.z,{color:"primary",size:L?"large":"medium",disabled:0===eg.length,onClick:function(){var e=new Set;eu(eg.reduce(function(a,i){return e.has(i.value)||a.push(i.value),e.add(i.value),a},[]).map(function(e){return{email:e,role:ev.member}})),ed(t.CSV_TABLE)},children:ee.formatMessage(J.inviteMemberInviteNextButton)});return(0,u.jsxs)("div",{className:"flex flex-col gap-2",children:[eo===t.INITIAL&&(0,u.jsxs)("div",{className:"flex flex-col gap-3",children:[(0,u.jsxs)("div",{className:"flex items-start justify-between gap-4",children:[(0,u.jsxs)("div",{className:"flex flex-col gap-1",children:[(0,u.jsx)("label",{htmlFor:"upload-csv",id:"upload-csv-label",children:(0,u.jsx)(I.Z,(0,l._)({},J.importCSVLabel))}),(0,u.jsxs)("span",{className:"text-sm text-gray-500",children:[(0,u.jsx)(I.Z,(0,o._)((0,l._)({},q?J.helperCSVFormatHide:J.helperCSVFormatShow),{values:{link:function(e){return(0,u.jsx)("button",{className:"underline",onClick:function(){K(!q)},children:e})}}})),q&&(0,u.jsx)("pre",{className:"mt-4",children:"email,role\nuser1@company.com,member\nadmin@company.com,admin\nit@company.com,owner"})]})]}),(0,u.jsx)("input",{ref:Y,type:"file",accept:".csv,text/csv",id:"upload-csv",className:"hidden",onChange:eb}),(0,u.jsx)("button",{className:"rounded bg-gray-100 px-4 py-2 dark:bg-gray-800",type:"button",onClick:function(){return Y.current.click()},children:(0,u.jsx)(I.Z,(0,l._)({},J.importCSVButton))})]}),(0,u.jsxs)("div",{className:"flex flex-col gap-2",children:[(0,u.jsx)("label",{htmlFor:"add-member-emails",id:"add-emails-label",children:(0,u.jsx)(I.Z,(0,l._)({},J.addEmailsFieldName))}),(0,u.jsx)(V,{value:eg,onChange:ep})]})]}),eo===t.CSV_TABLE&&(0,u.jsxs)(u.Fragment,{children:[G&&(0,u.jsxs)("div",{className:"flex w-full items-center bg-yellow-100 p-4",children:[(0,u.jsx)(w.BJv,{className:"icon-sm mr-2 text-yellow-700"}),(0,u.jsx)("div",{className:"flex-grow text-yellow-700",children:(0,u.jsx)(I.Z,(0,l._)({},J.unspecifiedRoleDescription))})]}),(0,u.jsxs)(S.Z.Root,{size:"normal",children:[(0,u.jsxs)(S.Z.Header,{children:[(0,u.jsx)(S.Z.HeaderCell,{children:(0,u.jsx)(I.Z,(0,l._)({},J.inviteTableHeaderEmail))}),(0,u.jsx)(S.Z.HeaderCell,{className:"pl-3 dark:bg-gray-900",children:(0,u.jsx)(I.Z,(0,l._)({},J.inviteTableHeaderRole))}),(0,u.jsx)(S.Z.HeaderCell,{})]}),(0,u.jsx)(S.Z.Body,{children:ec.map(function(e,a){return(0,u.jsxs)(S.Z.Row,{children:[(0,u.jsx)(S.Z.Cell,{children:(0,u.jsx)(F.Z,{name:"email",type:"email",value:e.email,onChange:function(e){return ex(a,"email",e.target.value)},color:"neutral"})}),(0,u.jsx)(S.Z.Cell,{children:(0,u.jsxs)(C.Z.Root,{value:e.role,onValueChange:function(e){return ex(a,"role",e)},children:[(0,u.jsxs)(C.Z.Trigger,{children:[(0,u.jsx)(C.Z.Value,{}),(0,u.jsx)(C.Z.Icon,{})]}),(0,u.jsx)(C.Z.Portal,{children:(0,u.jsxs)(C.Z.Content,{className:"border border-token-surface-secondary",children:[(0,u.jsx)(C.Z.Item,{value:ev.member,children:(0,u.jsx)(I.Z,(0,l._)({},J.inviteRoleMember))}),ev.admin&&(0,u.jsx)(C.Z.Item,{value:ev.admin,children:(0,u.jsx)(I.Z,(0,l._)({},J.inviteRoleAdmin))}),ev.owner&&(0,u.jsx)(C.Z.Item,{value:ev.owner,children:(0,u.jsx)(I.Z,(0,l._)({},J.inviteRoleOwner))})]})})]})}),(0,u.jsx)(S.Z.Cell,{textAlign:"right",children:(0,u.jsx)(w.Ybf,{onClick:function(){return eM(a)},className:"icon-sm mr-2 cursor-pointer text-token-text-secondary"})})]},a)})})]})]}),T&&en&&(0,u.jsx)("div",{className:"mt-2 flex items-center text-left text-token-text-secondary",children:(0,u.jsx)(_.Z,{id:"resend-emails",checked:ei,onChange:function(e){return en(e.currentTarget.checked)},label:ee.formatMessage(J.resendEmailsFieldName)})}),eo===t.INITIAL&&X&&(0,u.jsx)("div",{className:"mb-4",children:(0,u.jsx)(z.Z,{children:(0,u.jsx)(I.Z,(0,l._)({},J.teamInviteModalNote))})}),(0,u.jsxs)("div",{className:"flex justify-end gap-2",children:[(0,u.jsx)(N.z,{color:"neutral",size:L?"large":"medium",onClick:function(){eo!==t.INITIAL?ed(t.INITIAL):(ew(),A())},children:eo===t.INITIAL?P:ee.formatMessage(J.inviteMemberInviteBackButton)}),eo===t.INITIAL?eN:eI]})]})}(n=t||(t={}))[n.INITIAL=0]="INITIAL",n[n.CSV_TABLE=1]="CSV_TABLE";var O=function(e){var a={member:"member"};return e&&(a.admin="admin",a.owner="owner"),a},J={importCSVLabel:{id:"adminPage.importCSVLabel",defaultMessage:"Import from CSV",description:"Name of label for import from CSV button"},helperCSVFormatHide:{id:"adminPage.helperCSVFormat",defaultMessage:'The file must include email and an optional role on each line. Roles can be "member", "admin", or "owner". <link>Hide Example</link>',description:"Instructions for formatting the import CSV, with a link to hide the example"},helperCSVFormatShow:{id:"adminPage.helperCSVFormat",defaultMessage:'The file must include email and an optional role on each line. Roles can be "member", "admin", or "owner". <link>Show Example</link>',description:"Instructions for formatting the import CSV, with a link to show an example"},importCSVButton:{id:"adminPage.importCSVButton",defaultMessage:"Upload",description:"Name of upload from CSV button"},addEmailsFieldName:{id:"adminPage.addEmailsFieldName.0",defaultMessage:"Emails",description:"Name of input for adding emails to workspace"},unspecifiedRoleDescription:{id:"adminPage.unspecifiedRoleDescription",defaultMessage:"Roles that were unspecified have been changed to Member.",description:"Description text for defaulting invite roles to member"},inviteTableHeaderEmail:{id:"adminPage.inviteTableHeaderEmail",defaultMessage:"Email",description:"Label for email column of invite table header"},inviteTableHeaderRole:{id:"adminPage.inviteTableHeaderRole",defaultMessage:"Role",description:"Label for role column of invite table header"},inviteRoleMember:{id:"adminPage.inviteRoleMember",defaultMessage:"Member",description:"Member role for workspace invite"},inviteRoleAdmin:{id:"adminPage.inviteRoleAdmin",defaultMessage:"Admin",description:"Admin role for workspace invite"},inviteRoleOwner:{id:"adminPage.inviteRoleOwner",defaultMessage:"Owner",description:"Owner role for workspace invite"},fileTooLargeWarning:{id:"adminPage",defaultMessage:"File is too large. Please upload a CSV file smaller than {maxSize}.",description:"Error when uploading a file that is too large"},fileIncorrectTypeWarning:{id:"adminPage",defaultMessage:"Please upload a CSV file.",description:"Error when uploading a file that is not a CSV"},resendEmailsFieldName:{id:"adminPage.resendEmailsFieldName",defaultMessage:"Resend emails for existing invites",description:"Name of checkbox input for resending emails to workspace"},teamInviteModalNote:{id:"adminPage.teamInviteModalNote",defaultMessage:"Users that accept invites will be included as additional seats on your next invoice.",description:"Note for team plan invites that additional seats will be charged"},inviteMessageLoading:{id:"adminPage.inviteMessageLoading",defaultMessage:"Sending out {numRows} invites, this may take a minute...",description:"Loading message when invites are being sent"},inviteSuccessToast:{id:"adminPage.inviteSuccessToast",defaultMessage:"Invited {numRows} {userStr} to {workspaceName}",description:"Message on successfully sending out invites"},inviteMemberInviteNextButton:{id:"adminPage.inviteMemberInviteNextButton",defaultMessage:"Next",description:"The title of the invite member modal next button"},inviteMemberInviteSubmitButton:{id:"adminPage.inviteMemberInviteSubmitButton",defaultMessage:"Send invites",description:"The title of the invite member modal submit button"},inviteMemberInviteSubmitButtonLoading:{id:"adminPage.inviteMemberInviteSubmitButtonLoading",defaultMessage:"Sending invites...",description:"Button label while inviting members"},inviteMemberInviteBackButton:{id:"adminPage.inviteMemberInviteBackButton",defaultMessage:"Back",description:"The title of the invite member modal back button"},inviteMessageFailure:{id:"adminPage.inviteMessageFailure",defaultMessage:"Unable to invite {emailsStr}",description:"Message that shows which emails were unsuccessful at being invited"}}}}]);