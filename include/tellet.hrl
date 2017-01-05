%% ------------------------------------------------------------------------
%% Copyright (c) 2014, Kook Maeng <kook.maeng@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%% ------------------------------------------------------------------------


%% Riak Storage Backends
-define(BACKEND_BITCASK_RESOURCE, <<"bitcask_resource">>).
-define(BACKEND_BITCASK_YEAR, <<"bitcask_year">>).
-define(BACKEND_BITCASK_HALFYEAR, <<"bitcask_halfyear">>).
-define(BACKEND_BITCASK_QUARTER, <<"bitcask_quarter">>).
-define(BACKEND_BITCASK_MONTH, <<"bitcask_month">>).
-define(BACKEND_BITCASK_WEEK, <<"bitcask_week">>).
-define(BACKEND_ELEVELDB_STORAGE, <<"eleveldb_storage">>).
-define(BACKEND_ELEVELDB_HISTORY, <<"eleveldb_history">>).
-define(BACKEND_MEMORY_WEEK, <<"memory_week">>).
-define(BACKEND_MEMORY_DAY, <<"memory_day">>).

%% Riak Buckets
-define(BUCKET_CACHE, <<"ssam_cache">>).
-define(BUCKET_ACCOUNT_KEY, <<"ssam_account_key">>).


%% Common 
-define(DELIMITER, $:).

%% HTTP Status Codes
-define(STATUS_OK, 200).
-define(STATUS_CREATED, 201).
-define(STATUS_ACCEPTED, 202).
-define(STATUS_NO_CONTENT, 204).
-define(STATUS_NOT_MODIFIED, 304).
-define(STATUS_BAD_REQUEST, 400).
-define(STATUS_UNAUTHORIZED, 401).
-define(STATUS_PAYMENT_REQUIRED, 402).
-define(STATUS_FORBIDDEN, 403).
-define(STATUS_NOT_FOUND, 404).
-define(STATUS_METHOD_NOT_ALLOWED, 405).
-define(STATUS_CONFLICT, 409).
-define(STATUS_INTERNAL_SERVER_ERROR, 500).
-define(STATUS_NOT_IMPLEMENTED, 501).
-define(STATUS_SERVICE_UNAVAILABLE, 503).


%% Common Atoms
-define(nil, nil).
-define(undefined, undefined).
-define(undef, undefined).
-define(infinite, infinite).
-define(forever, forever).
-define(get, get).
-define(post, post).
-define(put, put).
-define(patch, patch).
-define(delete, delete).

%% Common Errors
-define(warning, warning).
-define(error, error).
-define(fatal, fatal).
-define(invalid, invalid).
-define(expired, expired).
-define(timeout, timeout).
-define(not_found, not_found).
-define(invalid_data, invalid_data).
-define(id_duplicated, id_duplicated).
-define(already_defined, already_defined).
-define(param_undefined, param_undefined).
-define(invalid_service, invalid_service).
-define(conflict, conflict).
-define(no_content, no_content).
-define(unknown_method, unknown_method).
-define(data_not_found, data_not_found).

%% Cowboy for REST
-define(ssam_http, ssam_http).
-define(ssam_https, ssam_https).

%% ssam_resource module
-define(invalid_collection_name, invalid_collection_name).
-define(invalid_resource_id, invalid_resource_id).
-define(duplicate_resource_id, duplicate_resource_id).
-define(collection_not_found, collection_not_found).
-define(item_not_found, item_not_found).
-define(resource_id_already_used, resource_id_already_used).
-define(resource_id_not_matched, resource_id_not_matched).

%% ssam_mail module
-define(mail_profile_not_found, mail_profile_not_found).
-define(nonce_not_matched, nonce_not_matched).

%% ssam_auth module
-define(invalid_key_encoded, invalid_key_encoded).
-define(invalid_key_updated, invalid_key_updated).
-define(key_expired, key_expired).
-define(token_not_matched, token_not_matched).

