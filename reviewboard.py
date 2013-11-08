from rbtools.api.client import RBClient
import pdb
import os
import dateutil.parser


username = os.getenv('USER')


log_file = open('/str/development/projects/open-source/emacs-reviewboard/debug.log', 'a+')


client = RBClient('http://localhost:8000/')
# client = RBClient('http://reviewboard.sgg.cisco.com/')
root = client.get_root()

def test():
    result = []
    filename = 'file1.py'
    review_request = root.get_review_request(review_request_id=12)
    for file in review_request.get_diff_context().files:
        if filename == file['dest_filename']:
            for ccdata in file['comment_counts']:
                for cdata in ccdata['comments']:
                    comment = []
                    import pdb
                    pdb.set_trace()
                    comment.append(cdata['line'])
                    comment.append(cdata['text'])
                    result.append(comment)
    return result

# print test()
# requests = root.get_review_requests()
# print requests
# pdb.set_trace()
# log_file.write('test\n')
# exit()


from epc.server import EPCServer

server = EPCServer(('localhost', 0))

@server.register_function
def outgoing_requests():
    return collect_requests(
        root.get_review_requests(
            from_user=username))


@server.register_function
def incomming_requests():
    return collect_requests(
        root.get_review_requests(
            to_users=username))


def collect_requests(requests):
    result = []
    for request in requests:
        row = []
        fields = request.fields
        row.append(fields['id'])
        summary = fields['summary']
        if not summary:
            summary = '[Draft]'
        row.append(summary)
        submitter = request.get_submitter()
        row.append(submitter.username)
        time_added = fields['time_added']
        row.append(date_to_str(time_added))
        row.append(fields['status'])
        result.append(row)
    return result

@server.register_function
def review(*id):
    id = id[0]
    review_request = root.get_review_request(review_request_id=id)
    result = []
    result.append(review_request['summary'])
    result.append(review_request['branch'])
    people = []
    for one in review_request['target_people']:
        people.append(one['title'])

    result.append(people)

    groups = []
    for group in review_request['target_groups']:
        groups.append(group)

    result.append(groups)

    repository = review_request.get_repository()
    result.append(repository['name'])

    result.append(review_request['description'])
    result.append(review_request['testing_done'])

    result.append(date_to_str(review_request['time_added']))

    return result

def date_to_str(date):
    datetime = dateutil.parser.parse(date)
    return datetime.strftime('%b. %d, %Y')

@server.register_function
def get_files(*id):
    id = id[0]
    result = []
    review_request = get_review_request(id)
    for file in review_request.get_diffs()[0].get_files():
        files = []
        files.append(file.fields['id'])
        files.append(file.fields['source_file'])
        result.append(files)

    return result

# return rr.get_diffs()[0].get_files()[0].get_patch()['data']

def get_review_request(id):
    return root.get_review_request(review_request_id=id)

@server.register_function
def get_comments(*data):
    review_id = data[0]
    filename = data[1]
    result = []
    review_request = get_review_request(review_id)
    #  rr.get_diff_context().files[1]['comment_counts'][0]['linenum']
    # for comment in review_request.get_diff_context().files
    # for file in review_request.get_diffs()[0].get_files():
    for file in review_request.get_diff_context().files:
        if filename == file['dest_filename']:
            for ccdata in file['comment_counts']:
                for cdata in ccdata['comments']:
                    comment = []
                    comment.append(cdata['line'])
                    comment.append(cdata['text'])
                    comment.append(cdata['user']['name'])
                    result.append(comment)
    return result


@server.register_function
def get_diff(*data):
    review_id = data[0]
    filename = data[1]
    log('Review_id: %s' % (review_id,))
    log('Filename: %s' % (filename,))
    review_request = get_review_request(review_id)
    diff = None
    for file in review_request.get_diffs()[0].get_files():
        if filename == file.fields['source_file']:
            diff = file.get_patch()['data']
    return diff
    # rr.get_diffs()[0].get_files()[0].get_patch()['data']
    # return review_request.get_diffs()[0].get_patch()['data']

def log(info):
    log_file.write("%s\n" % (info,))

# @server.register_function
def echo(*a):
    result = []
    for v in a:
        result.append(v * 10)
    return result

server.print_port()
server.serve_forever()

# pdb.set_trace()

if __name__ == '__main__':
    print outgoing_requests()
