-record (job_queue, {waitting = queue:new(),
                     processing = dict:new(),
                     done = [],
                     job_number = 1}).

% JobTime       :: second
% JobInfo       :: {JobNumber, JobTime, F}
% waitting_item :: JobInfo
% processing    :: {key, JobNumber, value, {JobTime, F}}
% done_item     :: JobInfo